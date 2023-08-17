package com.ishaan.apcsa_compiler.parse;

/**
 * This enum contains each possible {@link Token} kind.
 */
public enum TokenKind {
    DUMMY,

    EOF,

    // Single character tokens
    SEMICOLON,
    L_PAREN,
    R_PAREN,
    L_CURLY,
    R_CURLY,
    L_SQUARE,
    R_SQUARE,
    COMMA,
    DOT,

    // Operators
    PLUS,
    PLUS_PLUS,
    PLUS_EQUALS,
    MINUS,
    MINUS_MINUS,
    MINUS_EQUALS,
    STAR,
    STAR_EQUALS,
    SLASH,
    SLASH_EQUALS,
    PERCENT,
    PERCENT_EQUALS,
    EQUALS,
    EQUALS_EQUALS,
    EXCLAMATION,
    EXCLAMATION_EQUALS,
    LESS,
    LESS_EQUALS,
    GREATER,
    GREATER_EQUALS,
    AMPERSAND_AMPERSAND,
    BAR_BAR,

    // Literals
    INT_LITERAL,
    LONG_LITERAL /* Long literals are not part of the AP subset */,
    DOUBLE_LITERAL,
    FLOAT_LITERAL /* Float literals are not part of the AP subset */,
    ZERO_LITERAL, /* Special token kind for 0 literal */
    HEX_LITERAL /* Hex literals are not part of the AP subset */,
    BINARY_LITERAL /* Binary literals are not part of the AP subset */,
    OCTAL_LITERAL /* Octal literals are not part of the AP subset */,
    HEX_DOUBLE_LITERAL /* Hex double literals are not part of the AP subset */,
    CHAR_LITERAL /* Char literals are not part of the AP subset */,
    STRING_LITERAL,

    // Keywords
    KW_NOT_IN_SUBSET /* Token type for each Java keyword that is not in the subset */,

    KW_BOOLEAN,
    KW_CLASS,
    KW_DO,
    KW_DOUBLE,
    KW_ELSE,
    KW_EXTENDS,
    KW_FINAL,
    KW_FOR,
    KW_IF,
    KW_IMPLEMENTS,
    KW_IMPORT,
    KW_INT,
    KW_INTERFACE,
    KW_NEW,
    KW_PACKAGE,
    KW_PRIVATE,
    KW_PUBLIC,
    KW_RETURN,
    KW_STATIC,
    KW_SUPER,
    KW_THIS,
    KW_VOID,
    KW_WHILE,
    // Identifier
    ID,
}
