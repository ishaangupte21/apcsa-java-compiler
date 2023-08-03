package com.ishaan.apcsa_compiler.errors;

/**
 * This enum represents each of the possible error types that can be encountered by the compiler.
 */
public enum ErrorKind {
    ILLEGAL_EOF_MULTILINE_COMMENT,
    ILLEGAL_OPERATOR_AMPERSAND,
    ILLEGAL_OPERATOR_BAR,
    ILLEGAL_CHAR_AFTER_NUMERIC_SEPARATOR,
    ILLEGAL_LONG_LITERAL,
    ILLEGAL_FLOAT_LITERAL,
    NUMERIC_SEPARATOR_AFTER_FLOATING_POINT,
    NO_DIGIT_AFTER_EXPONENT_INDICATOR,
}
