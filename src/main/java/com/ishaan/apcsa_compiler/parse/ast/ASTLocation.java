package com.ishaan.apcsa_compiler.parse.ast;

import com.ishaan.apcsa_compiler.parse.Token;
import com.ishaan.apcsa_compiler.source.SourceFile;

/**
 * This object represents an AST node's location within the source.
 */
public class ASTLocation {
    private final long relativeStart, absoluteStart;

    private int size;

    private final SourceFile srcFile;

    public ASTLocation(long relativeStart, long absoluteStart, int size, SourceFile srcFile) {
        this.relativeStart = relativeStart;
        this.absoluteStart = absoluteStart;
        this.size = size;
        this.srcFile = srcFile;
    }

    public long getRelativeStart() {
        return relativeStart;
    }

    public long getAbsoluteStart() {
        return absoluteStart;
    }

    public int getSize() {
        return size;
    }

    public SourceFile getSrcFile() {
        return srcFile;
    }

    /**
     * This method "adds" two {@link ASTLocation} instances by combining their size from the starting point.
     * This is used in order to combine locations for expression and statement AST nodes.
     * In order to avoid creating a large number of unnecessary instances, we will edit the current instance instead of making a new one.
     *
     * @param rhs The {@link ASTLocation} instance to be added to this instance.
     * @return The combined instance of the two location nodes.
     */
    public ASTLocation add(ASTLocation rhs) {
        // We need to make sure that the RHS location is indeed after the LHS location.
        assert (rhs.relativeStart > this.relativeStart);

        // First, we need to compute the end position of the RHS.
        long rhsEndPosition = rhs.relativeStart + rhs.size;

        // Now, we need to compute the new size.

        // Finally, we can combine the two to edit the current instance.
        this.size = (int) (rhsEndPosition - this.relativeStart);

        return this;
    }

    public static ASTLocation fromToken(Token tok, SourceFile srcFile) {
        return new ASTLocation(tok.getRelativeStartPos(), tok.getAbsoluteStartPos(), tok.getSize(), srcFile);
    }
}
