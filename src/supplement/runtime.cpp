#include "runtime.h"
#include "../syntax.h"
#include "../utils.h"



namespace Runtime {

    // indexed directly by DataTypeEnum
    _TypeInfo primitives[] = {

        { DT_VOID, 0, 1 },

        { DT_I8, 1, 1 },
        { DT_I16, 2, 2 },
        { DT_I32, 4, 4 },
        { DT_I64, 8, 8 },

        { DT_U8, 1, 1 },
        { DT_U16, 2, 2 },
        { DT_U32, 4, 4 },
        { DT_U64, 8, 8 },

        { DT_F32, 4, 4 },
        { DT_F64, 8, 8 },

        { DT_STRING, 16, 8 },

        { DT_POINTER, 8, 8 },

        { DT_ARRAY, 16, 8 },
        { DT_SLICE, 16, 8 },

    };

    _ArrayInfo primitiveArrayTemplates[] = {

        {
            .base = { DT_ARRAY, 16, 8 },
            .elementType = &primitives[DT_VOID]
        },

        {
            .base = { DT_ARRAY, 16, 8 },
            .elementType = &primitives[DT_I8]
        },

        {
            .base = { DT_ARRAY, 16, 8 },
            .elementType = &primitives[DT_I16]
        },

        {
            .base = { DT_ARRAY, 16, 8 },
            .elementType = &primitives[DT_I32]
        },

        {
            .base = { DT_ARRAY, 16, 8 },
            .elementType = &primitives[DT_I64]
        },

        {
            .base = { DT_ARRAY, 16, 8 },
            .elementType = &primitives[DT_U8]
        },

        {
            .base = { DT_ARRAY, 16, 8 },
            .elementType = &primitives[DT_U16]
        },

        {
            .base = { DT_ARRAY, 16, 8 },
            .elementType = &primitives[DT_U32]
        },

        {
            .base = { DT_ARRAY, 16, 8 },
            .elementType = &primitives[DT_U64]
        },

        {
            .base = { DT_ARRAY, 16, 8 },
            .elementType = &primitives[DT_F32]
        },

        {
            .base = { DT_ARRAY, 16, 8 },
            .elementType = &primitives[DT_F64]
        },

    };

    _ArrayInfo primitiveStringTemplates[] = {

        {
            .base = { DT_STRING, 16, 8 },
            .elementType = &primitives[DT_U8]
        },

        {
            .base = { DT_STRING, 16, 8 },
            .elementType = &primitives[DT_U16]
        },

        {
            .base = { DT_STRING, 16, 8 },
            .elementType = &primitives[DT_U32]
        },

        {
            .base = { DT_STRING, 16, 8 },
            .elementType = &primitives[DT_U64]
        }

    };

    void init() {
    }

    _TypeInfo* getType(Variable* var) {

        const Value* val = &var->cvalue;
        const DataTypeEnum dtype = val->dtypeEnum;

        if (isPrimitive(dtype)) {
            return &Runtime::primitives[dtype];
        }

        if (dtype == DT_CUSTOM) {
            if (val->def->dtype.runtimeInfo) {
                return (_TypeInfo*) val->def->dtype.runtimeInfo;
            }
            return Runtime::resolve(&val->def->dtype, val->def);
        }

        // TODO
        if (dtype == DT_STRING) {
            // LOOK : strings should refer only to literlas
            //        so we have to be safe to cook like that
            StringInitialization* init = (StringInitialization*) var->expression;
            return (_TypeInfo*) &Runtime::primitiveStringTemplates[init->wideDtype - DT_U8];
        }

        return Runtime::resolve(dataTypes + dtype, val->any);

    }

    // AST -> Runtime Metadata
    // TODO : payload? is it ritght name?
    _TypeInfo* resolve(DataType* astType, void* payload) {

        if (!astType) return NULL;

        if (astType->runtimeInfo) {
            return (_TypeInfo*) astType->runtimeInfo;
        }


        switch (astType->kind) {

            case DT_CUSTOM: {

                TypeDefinition* typeDef = (TypeDefinition*) payload;// *(TypeDefinition**) DArray::get(&typeDefinitions, astType->id);

                _StructInfo* info = (_StructInfo*) alloc(alc, sizeof(_StructInfo), 8);
                info->base.kind = astType->kind;
                info->base.size = astType->size;
                info->base.align = astType->align;
                info->name.buff = typeDef->name.buff; // TODO : maybe copy
                info->name.len = typeDef->name.len;

                int memberCount = typeDef->vars.base.size;
                info->memberCount = memberCount;

                // TODO: will this be always aligned?
                info->members = (_StructMemberInfo*) alloc(alc, sizeof(_StructMemberInfo) * memberCount, 8);

                int offset = 0;
                for (int i = 0; i < memberCount; i++) {

                    _StructMemberInfo* dest = info->members + i;
                    Variable* src = *(Variable**) DArray::get(&typeDef->vars.base, i);

                    dest->name.buff = src->name.buff; // TODO : do we copy?
                    dest->name.len = src->name.len;

                    dest->info = getType(src);

                    offset = Utils::alignForward(offset, dest->info->align);
                    dest->offset = offset;
                    offset += dest->info->size;

                }

                astType->runtimeInfo = info;
                return (_TypeInfo*) info;

            }

            case DT_POINTER: {

                Pointer* ptr = (Pointer*) payload;
                if (isPrimitive(ptr->pointsToEnum)) {
                    return (_TypeInfo*) (&primitiveArrayTemplates[ptr->pointsToEnum]);
                }

                _PointerInfo* info = (_PointerInfo*) alloc(alc, sizeof(_PointerInfo));
                info->base = primitives[DT_POINTER];
                info->elementType = resolve(dataTypes + ptr->pointsToEnum, ptr->pointsTo);

                return (_TypeInfo*) info;

            }

            case DT_ARRAY: {
            
                Array* arr = (Array*) payload;
                if (isPrimitive(arr->base.pointsToEnum)) {
                    return (_TypeInfo*) (&primitiveArrayTemplates[arr->base.pointsToEnum]);
                }

                _ArrayInfo* info = (_ArrayInfo*) alloc(alc, sizeof(_ArrayInfo));
                info->base = primitives[DT_ARRAY];
                info->elementType = resolve(dataTypes + arr->base.pointsToEnum, arr->base.pointsTo);

                return (_TypeInfo*) info;
            }

            // TODO

            default: {

                return primitives + astType->kind;

            }

        }

    }

}
