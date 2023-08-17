package com.ishaan.apcsa_compiler.parse;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * This class serves as an interface for determining whether identifiers are keywords.
 */
public class Keywords {

    // This hash table contains a list of each keyword
    private static final Map<String, TokenKind> keywords = new HashMap<String, TokenKind>() {{
        put("abstract", TokenKind.KW_NOT_IN_SUBSET);
        put("assert", TokenKind.KW_NOT_IN_SUBSET);
        put("boolean", TokenKind.KW_BOOLEAN);
        put("break", TokenKind.KW_NOT_IN_SUBSET);
        put("byte", TokenKind.KW_NOT_IN_SUBSET);
        put("case", TokenKind.KW_NOT_IN_SUBSET);
        put("char", TokenKind.KW_NOT_IN_SUBSET);
        put("class", TokenKind.KW_CLASS);
        put("const", TokenKind.KW_NOT_IN_SUBSET);
        put("continue", TokenKind.KW_NOT_IN_SUBSET);
        put("default", TokenKind.KW_NOT_IN_SUBSET);
        put("do", TokenKind.KW_DO);
        put("double", TokenKind.KW_DOUBLE);
        put("else", TokenKind.KW_ELSE);
        put("enum", TokenKind.KW_NOT_IN_SUBSET);
        put("extends", TokenKind.KW_EXTENDS);
        put("final", TokenKind.KW_FINAL);
        put("finally", TokenKind.KW_NOT_IN_SUBSET);
        put("float", TokenKind.KW_NOT_IN_SUBSET);
        put("for", TokenKind.KW_FOR);
        put("if", TokenKind.KW_IF);
        put("goto", TokenKind.KW_NOT_IN_SUBSET);
        put("implements", TokenKind.KW_IMPLEMENTS);
        put("import", TokenKind.KW_IMPORT);
        put("instanceof", TokenKind.KW_NOT_IN_SUBSET);
        put("int", TokenKind.KW_INT);
        put("interface", TokenKind.KW_INTERFACE);
        put("long", TokenKind.KW_NOT_IN_SUBSET);
        put("native", TokenKind.KW_NOT_IN_SUBSET);
        put("new", TokenKind.KW_NEW);
        put("package", TokenKind.KW_PACKAGE);
        put("private", TokenKind.KW_PRIVATE);
        put("protected", TokenKind.KW_NOT_IN_SUBSET);
        put("public", TokenKind.KW_PUBLIC);
        put("return", TokenKind.KW_RETURN);
        put("short", TokenKind.KW_NOT_IN_SUBSET);
        put("static", TokenKind.KW_STATIC);
        put("strictfp", TokenKind.KW_NOT_IN_SUBSET);
        put("super", TokenKind.KW_SUPER);
        put("switch", TokenKind.KW_NOT_IN_SUBSET);
        put("synchronized", TokenKind.KW_NOT_IN_SUBSET);
        put("this", TokenKind.KW_THIS);
        put("throw", TokenKind.KW_NOT_IN_SUBSET);
        put("throws", TokenKind.KW_NOT_IN_SUBSET);
        put("transient", TokenKind.KW_NOT_IN_SUBSET);
        put("try", TokenKind.KW_NOT_IN_SUBSET);
        put("void", TokenKind.KW_VOID);
        put("volatile", TokenKind.KW_NOT_IN_SUBSET);
        put("while", TokenKind.KW_WHILE);
    }};

    /**
     * This method determines whether the given text constitutes a Java keyword.
     *
     * @param tokenText The string contents of the token.
     * @return An {@link Optional} instance containing a value of a keyword is identified.
     */
    public static Optional<TokenKind> checkForKeyword(String tokenText) {
        return Optional.ofNullable(keywords.get(tokenText));
    }
}
