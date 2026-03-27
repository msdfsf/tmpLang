#include "runtime.h"
#include "../syntax.h"
#include "../utils.h"



namespace Runtime {

    // indexed directly by DataTypeEnum
    _TypeInfo primitives[] = {

        { Type::DT_VOID, 0, 1 },

        { Type::DT_I8, 1, 1 },
        { Type::DT_I16, 2, 2 },
        { Type::DT_I32, 4, 4 },
        { Type::DT_I64, 8, 8 },

        { Type::DT_U8, 1, 1 },
        { Type::DT_U16, 2, 2 },
        { Type::DT_U32, 4, 4 },
        { Type::DT_U64, 8, 8 },

        { Type::DT_F32, 4, 4 },
        { Type::DT_F64, 8, 8 },

        { Type::DT_STRING, 16, 8 },

        { Type::DT_POINTER, 8, 8 },

        { Type::DT_ARRAY, 16, 8 },
        { Type::DT_SLICE, 16, 8 },

    };

    _ArrayInfo primitiveArrayTemplates[] = {

        {
            .base = { Type::DT_ARRAY, 16, 8 },
            .element = &primitives[Type::DT_VOID]
        },

        {
            .base = { Type::DT_ARRAY, 16, 8 },
            .element = &primitives[Type::DT_I8]
        },

        {
            .base = { Type::DT_ARRAY, 16, 8 },
            .element = &primitives[Type::DT_I16]
        },

        {
            .base = { Type::DT_ARRAY, 16, 8 },
            .element = &primitives[Type::DT_I32]
        },

        {
            .base = { Type::DT_ARRAY, 16, 8 },
            .element = &primitives[Type::DT_I64]
        },

        {
            .base = { Type::DT_ARRAY, 16, 8 },
            .element = &primitives[Type::DT_U8]
        },

        {
            .base = { Type::DT_ARRAY, 16, 8 },
            .element = &primitives[Type::DT_U16]
        },

        {
            .base = { Type::DT_ARRAY, 16, 8 },
            .element = &primitives[Type::DT_U32]
        },

        {
            .base = { Type::DT_ARRAY, 16, 8 },
            .element = &primitives[Type::DT_U64]
        },

        {
            .base = { Type::DT_ARRAY, 16, 8 },
            .element = &primitives[Type::DT_F32]
        },

        {
            .base = { Type::DT_ARRAY, 16, 8 },
            .element = &primitives[Type::DT_F64]
        },

    };

    _ArrayInfo primitiveStringTemplates[] = {

        {
            .base = { Type::DT_STRING, 16, 8 },
            .element = &primitives[Type::DT_U8]
        },

        {
            .base = { Type::DT_STRING, 16, 8 },
            .element = &primitives[Type::DT_U16]
        },

        {
            .base = { Type::DT_STRING, 16, 8 },
            .element = &primitives[Type::DT_U32]
        },

        {
            .base = { Type::DT_STRING, 16, 8 },
            .element = &primitives[Type::DT_U64]
        }

    };

    void init() {
    }

    _TypeInfo* getType(Variable* var) {

        const Value* val = &var->value;
        const Type::Kind dtype = val->typeKind;

        if (isPrimitive(dtype)) {
            return &Runtime::primitives[dtype];
        }

        if (dtype == Type::DT_CUSTOM) {
            if (val->def->typeInfo) {
                return (_TypeInfo*) val->def->typeInfo;
            }
            return Runtime::resolve((Type::TypeInfoEx*) &val->def->typeInfo->base, val->def);
        }

        // TODO
        if (dtype == Type::DT_STRING) {
            // LOOK : strings should refer only to literlas
            //        so we have to be safe to cook like that
            StringInitialization* init = (StringInitialization*) var->expression;
            return (_TypeInfo*) &Runtime::primitiveStringTemplates[init->wideType - Type::DT_U8];
        }

        return Runtime::resolve((Type::TypeInfoEx*) Type::basicTypes + dtype, val->any);

    }

    // AST -> Runtime Metadata
    // TODO : payload? is it ritght name?
    _TypeInfo* resolve(Type::TypeInfoEx* astType, void* payload) {

        if (!astType) return NULL;

        if (astType) {
            return (_TypeInfo*) astType;
        }


        switch (astType->base.kind) {

            case Type::DT_CUSTOM: {

                TypeDefinition* typeDef = (TypeDefinition*) payload;// *(TypeDefinition**) DArray::get(&typeDefinitions, astType->id);

                _StructInfo* info = (_StructInfo*) alloc(alc, sizeof(_StructInfo), 8);
                info->base.kind = astType->base.kind;
                info->base.size = astType->base.size;
                info->base.align = astType->base.align;
                info->name.buff = typeDef->name.buff; // TODO : maybe copy
                info->name.len = typeDef->name.len;

                int memberCount = typeDef->varCount;
                info->memberCount = memberCount;

                // TODO: will this be always aligned?
                info->members = (_StructMemberInfo*) alloc(alc, sizeof(_StructMemberInfo) * memberCount, 8);

                int offset = 0;
                for (int i = 0; i < memberCount; i++) {

                    _StructMemberInfo* dest = info->members + i;
                    Variable* src = typeDef->vars[i];

                    dest->name.buff = src->name.buff; // TODO : do we copy?
                    dest->name.len = src->name.len;

                    dest->type = getType(src);

                    offset = Utils::alignForward(offset, dest->type->align);
                    dest->offset = offset;
                    offset += dest->type->size;

                }

                astType->str = *info;
                return (_TypeInfo*) info;

            }

            case Type::DT_POINTER: {

                Pointer* ptr = (Pointer*) payload;
                if (isPrimitive(ptr->pointsToKind)) {
                    return (_TypeInfo*) (&primitiveArrayTemplates[ptr->pointsToKind]);
                }

                _PointerInfo* info = (_PointerInfo*) alloc(alc, sizeof(_PointerInfo));
                info->base = primitives[Type::DT_POINTER];
                info->element = resolve((_TypeInfoEx*) Type::basicTypes + ptr->pointsToKind, ptr->pointsTo);

                return (_TypeInfo*) info;

            }

            case Type::DT_ARRAY: {

                Array* arr = (Array*) payload;
                if (isPrimitive(arr->base.pointsToKind)) {
                    return (_TypeInfo*) (&primitiveArrayTemplates[arr->base.pointsToKind]);
                }

                _ArrayInfo* info = (_ArrayInfo*) alloc(alc, sizeof(_ArrayInfo));
                info->base = primitives[Type::DT_ARRAY];
                info->element = resolve((_TypeInfoEx*) Type::basicTypes + arr->base.pointsToKind, arr->base.pointsTo);

                return (_TypeInfo*) info;
            }

            // TODO

            default: {

                return primitives + astType->base.kind;

            }

        }

    }

}
