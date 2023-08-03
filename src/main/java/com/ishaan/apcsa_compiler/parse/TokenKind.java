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
}
