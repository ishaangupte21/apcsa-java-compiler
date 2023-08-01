package com.ishaan.apcsa_compiler.util;

/**
 * Tuple pair API to be used throughout the compiler.
 */
public class Pair<T, S> {
    protected final T first;
    protected final S second;

    public Pair(T first, S second) {
        this.first = first;
        this.second = second;
    }

    public T getFirst() {
        return first;
    }

    public S getSecond() {
        return second;
    }
}
