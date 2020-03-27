#include "Lexer.hpp"

/*
 * Function to return the next token from standard input
 * the variable 'm_IdentifierStr' is set there in case of an identifier,
 * the variable 'm_NumVal' is set there in case of a number.
 */

int Lexer::gettok() {
    m_NumVal = 42; 
    return tok_number;
}

