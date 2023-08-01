package com.ishaan.apcsa_compiler.source;

/**
 * This is the exception that will be thrown when Java source file sizes are larger than 32 bits.
 * It has no members or methods.
 */
public class SourceFileTooLargeException extends Exception {
    private SourceFileTooLargeException() {
    }

    public static final SourceFileTooLargeException INSTANCE = new SourceFileTooLargeException();
}
