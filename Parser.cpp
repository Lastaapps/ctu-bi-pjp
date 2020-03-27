#include "Parser.hpp"

Parser::Parser() : SfeContext(), SfeBuilder(SfeContext), SfeModule("sfe", SfeContext) {
}

bool Parser::Parse() {
    getNextToken();
    return true;
}

const Module& Parser::Generate() {

    // create writeln function
    {
      std::vector<Type*> Ints(1, Type::getInt32Ty(SfeContext));
      FunctionType * FT = FunctionType::get(Type::getInt32Ty(SfeContext), Ints, false);
      Function * F = Function::Create(FT, Function::ExternalLinkage, "writeln", SfeModule);
      for (auto & Arg : F->args())
          Arg.setName("x");
    }

    // create main function
    {
      FunctionType * FT = FunctionType::get(Type::getInt32Ty(SfeContext), false);
      Function * MainFunction = Function::Create(FT, Function::ExternalLinkage, "main", SfeModule);

      // block
      BasicBlock * BB = BasicBlock::Create(SfeContext, "entry", MainFunction);
      SfeBuilder.SetInsertPoint(BB);

      // call writeln with value from lexel
      SfeBuilder.CreateCall(SfeModule.getFunction("writeln"), {
        ConstantInt::get(SfeContext, APInt(32, m_Lexer.numVal()))
      });

      // return 0
      SfeBuilder.CreateRet(ConstantInt::get(Type::getInt32Ty(SfeContext), 0));
    }

    return this->SfeModule;
}

/*
 * Simple token buffer.
 * CurTok is the current token the parser is looking at
 * getNextToken reads another token from the lexer and updates curTok with ts result
 * Every function in the parser will assume that CurTok is the cureent token that needs to be parsed
 */

int Parser::getNextToken() {
    return CurTok = m_Lexer.gettok();
}
