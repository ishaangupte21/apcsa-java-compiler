package com.ishaan.apcsa_compiler;

import com.ishaan.apcsa_compiler.parse.Lexer;
import com.ishaan.apcsa_compiler.parse.Parser;
import com.ishaan.apcsa_compiler.parse.Token;
import com.ishaan.apcsa_compiler.parse.ast.ASTCompilationUnit;
import com.ishaan.apcsa_compiler.source.*;

import java.io.IOException;

/*
    This is the main entry point for the compiler.
 */
public class Main {
    public static void main(String[] args) {
        // If no source file is given, we will exit the program.
        if (args.length == 0) {
            System.err.println("error: no source files given");
            System.exit(1);
        }

        try {
            SourceFile srcFile = SourceMap.bufferSrcFile(args[0]);
            Lexer lexer = new Lexer(srcFile);
            Parser parser = new Parser(lexer);

            ASTCompilationUnit tree = parser.parseCompilationUnit();
            System.out.println(tree);

        } catch (Exception e) {
            if (e instanceof IOException) {
                System.err.printf("error: %s\n", e.getLocalizedMessage());
            } else if (e instanceof SourceFileTooLargeException) {
                System.err.printf("error: file %s is too large\n", args[0]);
            } else if (e instanceof UnicodeReadingException) {
                System.err.println("error: invalid UTF-8 encoding.");
            } else {
                System.err.println(e.getLocalizedMessage());
            }

            System.exit(1);
        }
    }
}
