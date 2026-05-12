#include "../../src/array_list.h"
#include "../../src/file_system.h"
#include "../../src/parser.h"
#include "../../src/registry.h"
#include "../../src/config.h"
#include "../../src/translator.h"
#include "lsp.h"

#include <atomic>
#include <cstdint>
#include <cstring>
#include <thread>



namespace TaskSystem {

    enum State {
        TS_READY,
        TS_RUNNING,
        TS_PENDING
    };

    enum TaskKind : uint8_t {
        TK_PARSING,
        TK_VALIDATION,
    };

    union TaskState {
        // TODO : better name?
        Parser::ParseContext p;
        // Validator::ValidateContext v;
    };

    union TaskArgument {
        // TK_PARSING
        FileSystem::Handle file;

        // TK_VALIDATION
        Reg::Unit*         unit;

        // generic
        void*              ptr;
        uint64_t           val;
    };

    typedef void TaskFunction (TaskState*, TaskArgument);

    struct Task {
        TaskFunction* fcn;
        TaskArgument  arg;
        TaskKind      kind;
    };

    struct Worker {
        int id;

        std::thread thread;
        std::atomic<bool> hasWork;

        TaskState state;
        DArray::Container queue;
    };

    static Worker*  gWorkers = nullptr;
    static uint32_t gWorkerCount = 0;

    static thread_local Worker* gCurrentWorker = nullptr;

    // The 'check list' mechanism, each thread
    // will use its own bit to write to mark if
    // its ready for the next task or not.
    // 1 - free, 0 - working
    std::atomic<uint64_t> checkList;

    // Internal counter to track the "Group" so Wait() knows when to stop
    static std::atomic<int32_t> gTaskCount = 0;



    inline int findFirstSetBit(uint64_t mask) {
        #if defined(_MSC_VER)
            unsigned long index;
            return _BitScanForward64(&index, mask) ? (int)index : -1;
        #else
            return (mask == 0) ? -1 : __builtin_ctzll(mask);
        #endif
    }

    void clearTaskState(TaskState* state, TaskKind kind) {

        if (kind == TK_PARSING) {
            Parser::ParseContext* ctx = &state->p;

            ctx->unit            = NULL;
            ctx->fileSpan        = NULL;
            ctx->currentScope    = NULL;
            ctx->currentFunction = NULL;
            ctx->currentLoop     = NULL;
            ctx->currentImport   = NULL;

            DArray::clear(&ctx->nodeStack);
            DArray::clear(&ctx->defStack);
            Arena::clear(&ctx->errBuff);

            ctx->varId &= THREAD_MASK;
            ctx->arrId &= THREAD_MASK;
            ctx->defId &= THREAD_MASK;
            ctx->errId &= THREAD_MASK;

            ctx->idxInScope = 0;
        } else {
            memset(state, 0, sizeof(TaskState));
        }

    }

    // returns free worker or NULL at failure
    Worker* secureWorker() {
        constexpr auto memorder = std::memory_order_relaxed;

        // for future me: memory_order_relaxed -
        //  only this operation's atomicity is guaranteed
        uint64_t checkListCopy = checkList.load(memorder);

        while (checkListCopy != 0) {
            int workerId = findFirstSetBit(checkListCopy);
            if (workerId < 0) break;

            // fetch_and returns the value of checkList
            // before the AND was applied
            const uint64_t mask = 1ULL << workerId;
            if (checkList.fetch_and(~mask) & mask) {
                return gWorkers + workerId;
            }

            checkListCopy = checkList.load(memorder);
        }

        return NULL;
    }

    void runWorker(Worker* worker) {
        gCurrentWorker = worker;

        Parser::init(&worker->state.p);

        uint64_t prefixID = (uint64_t) worker->id << 56;

        worker->state.p.varId = prefixID;
        worker->state.p.arrId = prefixID;
        worker->state.p.defId = prefixID;
        worker->state.p.errId = prefixID;

        while (1) {

            worker->hasWork.wait(false);

            while (worker->queue.size > 0) {

                Task* task = (Task*) DArray::getLast(&worker->queue);
                DArray::pop(&worker->queue);

                clearTaskState(&worker->state, task->kind);
                task->fcn(&worker->state, task->arg);

                // If we have more than one job to do,
                // we try to pass them to other workers
                while (worker->queue.size > 1) {
                    Worker* subWorker = secureWorker();
                    if (!subWorker) break;

                    Task* task = (Task*) DArray::getLast(&worker->queue);
                    DArray::pop(&worker->queue);

                    DArray::push(&subWorker->queue, task);
                    subWorker->hasWork.store(true);
                    subWorker->hasWork.notify_all();
                }

            }

            worker->hasWork.store(false);
            // worker->hasWork.notify_one();

            checkList.fetch_or(1ULL << worker->id);
            checkList.notify_all();

        }
    }

    void init(uint64_t workerCount) {
        if (workerCount == 0) {
            workerCount = std::thread::hardware_concurrency();
            if (workerCount == 0) workerCount = 1;
        }
        if (workerCount > 64) workerCount = 64;

        checkList.store((1ULL << workerCount) - 1);

        gWorkerCount = workerCount;
        gWorkers = (Worker*) Lsp::alloc<Worker>(&Lsp::State::allocator, workerCount);

        for (int i = 0 ; i < workerCount; i++) {
            Worker* worker = gWorkers + i;

            worker->id = i;
            worker->hasWork.store(false);
            worker->thread = std::thread(runWorker, worker);

            DArray::init(&worker->queue, Config::threadWorkQueueSize, sizeof(Task));

            worker->thread.detach();
        }
    }

    void enqueue(Task task) {
        Worker* worker = secureWorker();

        if (worker) {
            DArray::push(&worker->queue, &task);
            worker->hasWork.store(true);
            worker->hasWork.notify_one();
        } else {
            // Everyone is busy
            if (gCurrentWorker) {
                DArray::push(&gCurrentWorker->queue, &task);
            } else {
                // We are on the main thread, we will assign it
                // to a 'random' worker
                Worker* worker = gWorkers + 0;

                DArray::push(&worker->queue, &task);
                worker->hasWork.store(true);
                worker->hasWork.notify_one();
            }
        }
    }

    void setAndClearLocalArena(Arena::Container* arena) {
        alc = arena;
        nalc = arena;
        clear(alc);
    }

    static void runParse(TaskState* state, TaskArgument arg) {
        Lsp::FileData* data = (Lsp::FileData*) FileSystem::getUserData(arg.file);

        int committedIdx = data->committedIdx.load(std::memory_order_relaxed);
        int workingIdx = 1 - committedIdx;

        // We wait for all UI queries to finish reading buffer we want to modify.
        int readerCount = data->readerCount[workingIdx].load(std::memory_order_acquire);
        while (readerCount > 0) {
            data->readerCount[workingIdx].wait(readerCount);
            readerCount = data->readerCount[workingIdx].load(std::memory_order_acquire);
        }

        setAndClearLocalArena(data->arenas + workingIdx);

        state->p.unit = Reg::get(arg.file);
        Parser::parse(&state->p, arg.file);

        data->committedIdx.store(workingIdx);

        FileSystem::FileInfo* finfo = FileSystem::getFileInfo(arg.file);
        finfo->status.store(FileSystem::FS_READY, std::memory_order_release);

        gTaskCount.fetch_sub(1, std::memory_order_relaxed);
        gTaskCount.notify_one();
    }

    void dispatchParse(FileSystem::Handle fhnd) {
        using namespace FileSystem;
        constexpr auto memorder = std::memory_order_relaxed;

        FileInfo* finfo = getFileInfo(fhnd);

        FileStatus expected = FS_DIRTY;
        if (!finfo->status.compare_exchange_strong(
            expected, FS_BUSY, memorder)) {
            return;
        }

        gTaskCount.fetch_add(1, std::memory_order_relaxed);

        Task task;
        task.arg.file = fhnd;
        task.kind = TK_PARSING;
        task.fcn = &runParse;

        enqueue(task);
    }

    void dispatchValidation(FileSystem::Handle file) {

    }

    void dispatchCodegen(Translator* translator, FileSystem::Handle file) {

    }

    void beginGroup() {
        gTaskCount.store(0, std::memory_order_seq_cst);
        std::atomic_thread_fence(std::memory_order_seq_cst);
    }

    void wait() {
        // Acquire ensures we see the finished ASTs produced by workers
        int32_t count = gTaskCount.load(std::memory_order_acquire);
        while (count > 0) {
            gTaskCount.wait(count);
            count = gTaskCount.load(std::memory_order_acquire);
        }
    }

    [[noreturn]] void panic(int code) {
        std::exit(code);
    }

}
