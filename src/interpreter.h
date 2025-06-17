#pragma once

#include "syntax.h"

namespace Interpreter {
    
    // returns error or dtype enumeric value of the result
    // stores ivalue and idtypeEnum into ans value and dtypeValue
    int execFunction(Function* fcn, Operand* ans);
    

    inline int evaluate(Operand* op);
    
    int applyOperator(OperatorEnum oper, Value* value);
    int applyOperator(OperatorEnum oper, Value* valueA, Value* valueB);

}
