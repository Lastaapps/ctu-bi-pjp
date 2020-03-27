#ifndef PJPPROJECT_LEXER_HPP
#define PJPPROJECT_LEXER_HPP

#include <iostream>

class Lexer {
public:
    Lexer() = default;
    ~Lexer() = default;

    int gettok();
    const std::string& identifierStr() const { return this->m_IdentifierStr; }
    int numVal() { return this->m_NumVal; }
private:
    std::string m_IdentifierStr;
    int m_NumVal;
};


/*
 * Lexer returns tokens [0-255] if it is an unknown character, 
 * otherwise one of these for known things.
 * Here are all valid tokens:
 */

enum Token {
    tok_eof =           -1,

    // numbers and identifiers
    tok_identifier =    -2,
    tok_number =        -3,

    // keywords
    tok_begin =         -4,
    tok_end =           -5,
    tok_const =         -6,
    tok_procedure =     -7,
    tok_forward =       -8,
    tok_function =      -9,
    tok_if =            -10,
    tok_then =          -11,
    tok_else =          -12,
    tok_program =       -13,
    tok_while =         -14,
    tok_exit =          -15,
    tok_var =           -16,
    tok_integer =       -17,
    tok_for =           -18,
    tok_do =            -19,

    // 2-character operators
    tok_notequal =      -20,
    tok_lessequal =     -21,
    tok_greaterequal =  -22,
    tok_assign =        -23,
    tok_or =            -24,

    // 3-character operators (keywords)
    tok_mod =           -25,
    tok_div =           -26,
    tok_not =           -27,
    tok_and =           -28,
    tok_xor =           -29,

    // keywords in for loop
    tok_to =            -30,
    tok_downto =        -31
};

#endif //PJPPROJECT_LEXER_HPP
