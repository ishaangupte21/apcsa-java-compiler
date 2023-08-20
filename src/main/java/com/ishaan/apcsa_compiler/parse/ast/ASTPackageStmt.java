package com.ishaan.apcsa_compiler.parse.ast;

import java.util.List;

/**
 * This class represents the AST node for a package declaration at the top of a Java source file.
 */
public class ASTPackageStmt extends ASTNode {
    public final List<ASTPackageFragment> packageNameFragments;

    public ASTPackageStmt(ASTLocation locus, List<ASTPackageFragment> packageNameFragments) {
        super(locus);
        this.packageNameFragments = packageNameFragments;
    }

    /**
     * This subclass represents a fragment that is separated by the DOT token.
     * Each fragment will have its own location info.
     */
    public static class ASTPackageFragment extends ASTNode {
        // This is the String text content of the fragment.
        public final String fragmentName;

        public ASTPackageFragment(ASTLocation locus, String fragmentName) {
            super(locus);
            this.fragmentName = fragmentName;
        }
    }
}
