package com.ishaan.apcsa_compiler.parse;

import com.ishaan.apcsa_compiler.errors.ErrorKind;
import com.ishaan.apcsa_compiler.errors.ErrorReporter;
import com.ishaan.apcsa_compiler.parse.ast.ASTCompilationUnit;
import com.ishaan.apcsa_compiler.parse.ast.ASTLocation;
import com.ishaan.apcsa_compiler.parse.ast.ASTPackageStmt;
import com.ishaan.apcsa_compiler.source.SourceFile;

import java.util.ArrayList;
import java.util.List;

/**
 * This class is the Parser which will read tokens from the Lexer and construct an AST.
 * This is an implementation of a Recursive Descent parser.
 */
public class Parser {

    // This is the lexer instance
    private final Lexer lexer;

    // This is the SourceFile instance for the file being parsed.
    private final SourceFile srcFile;

    // This is the token instance which will be passed to the lexer.
    private final Token tok = Token.dummy();

    public Parser(Lexer lexer) {
        this.lexer = lexer;
        this.srcFile = lexer.getSrcFile();
    }

    /**
     * This method gets the next token from the Lexer.
     */
    private void advance() {
        lexer.getNextToken(tok);
    }

    private boolean matches(TokenKind kind) {
        return tok.getKind() == kind;
    }

    /**
     * This method generates an {@link ASTLocation} instance from the current lookahead token.
     *
     * @return The {@link ASTLocation} instance from the current Parser token.
     */
    private ASTLocation locFromCurrentTok() {
        return ASTLocation.fromToken(tok, srcFile);
    }


    /**
     * This method calls the {@link ErrorReporter} method to report syntax errors.
     *
     * @param kind The error kind.
     */
    private void reportSyntaxError(ErrorKind kind) {
        ErrorReporter.reportWithLocalFilePosition(kind, tok.getRelativeStartPos(), tok.getSize(), srcFile);
    }

    /**
     * This method will recover from errors by disposing all tokens until a semicolon is found or the start of a new statement is found.
     */
    private void recoverUntilEndOfStmt() {
        // We must consume all tokens until we get a semicolon or EOF.
        advance();

        while (true) {
            switch (tok.getKind()) {
                case SEMICOLON:
                    // Consume the semicolon and return.
                    advance();
                    return;

                case EOF:
                    return;

                default:
                    advance();
                    break;
            }
        }
    }

    public ASTCompilationUnit parseCompilationUnit() {
        ASTCompilationUnit compilationUnit = new ASTCompilationUnit();

        // Load the first token
        advance();

        // At the top of the source file, we can have a package statement.
        if (matches(TokenKind.KW_PACKAGE)) {
            compilationUnit.packageStmt = parsePackageStmt();
        }


        return compilationUnit;
    }

    /**
     * This method parses package statements at the top of a Java source file.
     * PackageStmt = 'package' ID (. ID)*
     *
     * @return An {@link ASTPackageStmt} instance containing the identifier fragments, or null if parsing failed.
     */
    private ASTPackageStmt parsePackageStmt() {
        List<ASTPackageStmt.ASTPackageFragment> packageNameFragments = new ArrayList<>();

        ASTLocation stmtLocus = locFromCurrentTok();

        // In the first iteration of this loop, the 'package' keyword will be consumed.
        // In following iterations, all DOT tokens will be consumed.
        do {
            advance();
            if (!matches(TokenKind.ID)) {
                reportSyntaxError(ErrorKind.NO_ID_IN_PKG_STMT);
                return null;
            }

            // Store the current fragment location
            ASTLocation currentFragmentLocus = locFromCurrentTok();

            // Here, we need to construct a package fragment out of the current identifier.
            ASTPackageStmt.ASTPackageFragment currPackageFragment =
                    new ASTPackageStmt.ASTPackageFragment(currentFragmentLocus, tok.getLastStrValue());

            // Now, we can add the fragment to the list.
            packageNameFragments.add(currPackageFragment);

            // We also need to add on the location
            stmtLocus.add(currentFragmentLocus);

            // Consume the identifier
            advance();
        } while (matches(TokenKind.DOT));


        // Ending semicolon
        if (!matches(TokenKind.SEMICOLON)) {
            reportSyntaxError(ErrorKind.MISSING_SEMICOLON);
            // For error recovery, pretend this symbol exists.
        } else {
            // Consume the semicolon if it's found and add its location.
            stmtLocus.add(locFromCurrentTok());
            advance();
        }

        return new ASTPackageStmt(stmtLocus, packageNameFragments);
    }
}
