package com.ishaan.apcsa_compiler.parse.ast;

/**
 * This interface represents the base type for a node in the program Abstract Syntax Tree.
 */
public abstract class ASTNode {
    public final ASTLocation locus;

    public ASTNode(ASTLocation locus) {
        this.locus = locus;
    }
}
