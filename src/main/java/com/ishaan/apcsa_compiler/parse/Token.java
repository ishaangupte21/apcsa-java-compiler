package com.ishaan.apcsa_compiler.parse;

/**
 * This object represents a Token that will be scanned from the input by the Lexer.
 * In order to avoid cluttering the heap, there will be only one instance of Token for each source file.
 */
public class Token {
    private TokenKind kind;

    // This is the absolute starting position in the source map.
    private long absoluteStartPos;

    // This is the relative start position within the file.
    private long relativeStartPos;

    private int size;

    public TokenKind getKind() {
        return kind;
    }

    public long getAbsoluteStartPos() {
        return absoluteStartPos;
    }

    public long getRelativeStartPos() {
        return relativeStartPos;
    }

    public int getSize() {
        return size;
    }

    private Token() {
        this.kind = TokenKind.DUMMY;
        this.absoluteStartPos = -1;
        this.relativeStartPos = -1;
        this.size = -1;
    }

    public static Token dummy() {
        return new Token();
    }

    public void make(TokenKind kind, long absoluteStartPos, long relativeStartPos, int size) {
        this.kind = kind;
        this.absoluteStartPos = absoluteStartPos;
        this.relativeStartPos = relativeStartPos;
        this.size = size;
    }

    @Override
    public String toString() {
        return String.format("{ kind: %s, absoluteStartPos: %d, relativeStartPos: %d, size: %d }", kind, absoluteStartPos, relativeStartPos, size);
    }
}
