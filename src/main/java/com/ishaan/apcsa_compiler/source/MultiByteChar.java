package com.ishaan.apcsa_compiler.source;

import com.ishaan.apcsa_compiler.util.Pair;

/**
 * This object represents a single multibyte character from the source code.
 * This object is an instance of a Pair with two integer fields.
 */
public class MultiByteChar extends Pair<Integer, Integer> {

    public MultiByteChar(int codepoint, int length) {
        // We will initialize the two pair fields with the codepoint and length.
        super(codepoint, length);
    }

    public int getCodepoint() {
        return first;
    }

    public int getLength() {
        return second;
    }

    @Override
    public String toString() {
        return String.format("{ codepoint: %d, length: %d }", first, second);
    }
}
