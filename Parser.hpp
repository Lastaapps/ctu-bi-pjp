#ifndef PJPPROJECT_PARSER_HPP
#define PJPPROJECT_PARSER_HPP

#include "Lexer.hpp"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

using namespace llvm;

class Parser {
public:
    Parser();
    ~Parser() = default;

    bool Parse();             // parse
    const Module& Generate(); // generate

private:
    int getNextToken();
    
    Lexer m_Lexer;            // lexer is used to read tokens
    int CurTok;               // to keep the current token
    
    LLVMContext MilaContext;   // llvm context
    IRBuilder<> MilaBuilder;   // llvm builder
    Module MilaModule;         // llvm module
};

#endif //PJPPROJECT_PARSER_HPP
