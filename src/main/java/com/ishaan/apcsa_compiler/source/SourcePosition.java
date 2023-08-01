package com.ishaan.apcsa_compiler.source;

/**
 * This object represents a specific position within a source file.
 * It does not contain pointers into the buffer, but just the line and column.
 */
public class SourcePosition {
    private final String filePath;

    private final int line, col;

    public SourcePosition(String filePath, int line, int col) {
        this.filePath = filePath;
        this.line = line;
        this.col = col;
    }

    public String getFilePath() {
        return filePath;
    }

    public int getLine() {
        return line;
    }

    public int getCol() {
        return col;
    }

    @Override
    public String toString() {
        return String.format("{ filePath: '%s', line: %d, col: %d }\n", filePath, line, col);
    }
}
