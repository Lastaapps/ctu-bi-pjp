#include "Parser.hpp"

Parser::Parser()
    : MilaContext()
    , MilaBuilder(MilaContext)
    , MilaModule("mila", MilaContext)
{
}

bool Parser::Parse()
{
    getNextToken();
    return true;
}

const llvm::Module& Parser::Generate()
{

    // create writeln function
    {
      std::vector<llvm::Type*> Ints(1, llvm::Type::getInt32Ty(MilaContext));
      llvm::FunctionType * FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), Ints, false);
      llvm::Function * F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "writeln", MilaModule);
      for (auto & Arg : F->args())
          Arg.setName("x");
    }

    // create main function
    {
        llvm::FunctionType * FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), false);
        llvm::Function * MainFunction = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "main", MilaModule);

        // block
        llvm::BasicBlock * BB = llvm::BasicBlock::Create(MilaContext, "entry", MainFunction);
        MilaBuilder.SetInsertPoint(BB);

      // call writeln with value from lexel
      MilaBuilder.CreateCall(MilaModule.getFunction("writeln"), {
              llvm::ConstantInt::get(MilaContext, llvm::APInt(32, m_Lexer.numVal()))
      });

      // return 0
      MilaBuilder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(MilaContext), 0));
    }

    return this->MilaModule;
}

/**
 * @brief Simple token buffer.
 *
 * CurTok is the current token the parser is looking at
 * getNextToken reads another token from the lexer and updates curTok with ts result
 * Every function in the parser will assume that CurTok is the cureent token that needs to be parsed
 */
int Parser::getNextToken()
{
    return CurTok = m_Lexer.gettok();
}
