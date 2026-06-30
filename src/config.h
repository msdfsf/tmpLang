namespace Config {

    // Set to true for LSP/Library builds
    #if defined(CONFIG_ERROR_RECOVERY)
        constexpr bool ERROR_RECOVERY_ENABLED = true;
    #else
        constexpr bool ERROR_RECOVERY_ENABLED = false;
    #endif

    // Set to true to enable logging
    #if defined(CONFIG_DISABLE_LOGGING)
        constexpr bool LOGGING_ENABLED = false;
    #else
        constexpr bool LOGGING_ENABLED = true;
    #endif

    constexpr int LINEAR_SEARCH_THRESHOLD = 16;

    extern int maxErrorCount;

    extern int threadCount;
    extern int threadWorkQueueSize;

    // starting file count to pre-allocate space for
    extern int fileCount;

    // expected namespace count per file used
    // ex. to pre-allocate sets
    extern int expectedNamespaceCount;

}
