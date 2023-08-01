package com.ishaan.apcsa_compiler.source;

public class UnicodeReadingException extends Exception {
    private UnicodeReadingException() {
    }

    public static final UnicodeReadingException INSTANCE = new UnicodeReadingException();
}
