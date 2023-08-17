package com.ishaan.apcsa_compiler.errors;

import com.ishaan.apcsa_compiler.source.MultiByteChar;
import com.ishaan.apcsa_compiler.source.SourceFile;
import com.ishaan.apcsa_compiler.source.SourcePosition;

import java.util.List;
import java.util.Objects;

/**
 * This class serves as an interface for reporting error messages to the user through stderr.
 */
public class ErrorReporter {
    private static boolean hasSeenError = false;

    public static boolean isHasSeenError() {
        return hasSeenError;
    }

    /**
     * This file reports an error with local file position data to the user.
     *
     * @param kind          The type of the error.
     * @param localStartPos The local start position of the error within the source file.
     * @param size          The length of the offending text
     * @param srcFile       The local Source File where the error is located.
     */
    public static void reportWithLocalFilePosition(ErrorKind kind, long localStartPos, int size, SourceFile srcFile) {
        hasSeenError = true;

        // Obtain the error diagnosis message and the error's location.
        String errorDiagnosisMsg = errorDiagnosisMessages[kind.ordinal()];
        SourcePosition errLocus = srcFile.getSourcePosition(localStartPos);

        // Start by reporting the error its location
        System.err.printf("error: %s\n --> %s at line %d, column %d\n", errorDiagnosisMsg, errLocus.getFilePath(), errLocus.getLine(), errLocus.getCol());

        // Get the error line's text.
        String errLineText = srcFile.getLineText(errLocus.getLine());

        // Now, print the line text.
        int digitsInLine = getTotalDigitsInNumber(errLocus.getLine());

        String lineNumberPadding = getLineNumberPadding(digitsInLine);

        // Print the line text to the user.
        System.err.printf(" %s |\n %d | %s\n %s | ", lineNumberPadding, errLocus.getLine(), errLineText, lineNumberPadding);

        String lineStartPadding = getLineStartPadding(errLocus.getLine(), srcFile, localStartPos);

        // We need to figure out whether this error is across multiple lines.
        // If it is, we will just take the content of the first line.
        if (isErrorOverMultipleLines(localStartPos, size, srcFile)) {
            // If the error is over multiple lines, we need to truncate the offending text length to just the first line.
            StringBuilder truncatedOffendingTextMarker = new StringBuilder();
//            long lineEndPos = srcFile.getNewLineChars().get(line)

            System.err.printf("%s%s\n", lineStartPadding, truncatedOffendingTextMarker);
        } else {
            // If the error isn't over multiple lines, we can just compute the distance frm the line start to the start of the error..
            String offendingTextMarker = getOffendingTextMarker(localStartPos, size, srcFile);
            System.err.printf("%s%s\n", lineStartPadding, offendingTextMarker);
        }

        String errHelpMsg = errorHelpMessages[kind.ordinal()];
        System.err.printf("help: %s\n\n", errHelpMsg);
    }

    /**
     * This method determines whether the given error is over multiple lines.
     *
     * @param localPosition The local starting position of the error within the source file.
     * @param size          The length of the offending text.
     * @param srcFile       The source file which contains the error.
     * @return true if the error is over multiple lines, and false otherwise.
     */
    private static boolean isErrorOverMultipleLines(long localPosition, int size, SourceFile srcFile) {
        // If the source file has only 1 line, we know that the error can't be over multiple lines.
        if (srcFile.getNewLineChars().isEmpty())
            return false;

        int errStart = (int) localPosition;
        int errEnd = errStart + size;

        // We need to determine whether the source file has a Newline character within errStart and errEnd.
        List<Long> newLineChars = srcFile.getNewLineChars();

        for (Long currentNewLineChar : newLineChars) {
            // If the newline character position is less than the error start position, continue.
            if (currentNewLineChar < errStart)
                continue;

            // If it is greater, we need to make sure it is less than the end position.
            if (errEnd > currentNewLineChar)
                continue;

            return true;
        }

        return false;
    }

    /**
     * This method gets the space padding required at the of the line.
     *
     * @param line     The line number
     * @param srcFile  The source file where the line is located
     * @param errStart The start position of the error
     * @return The String containing the line padding
     */
    private static String getLineStartPadding(int line, SourceFile srcFile, long errStart) {
        StringBuilder builder = new StringBuilder();
        long lineStartPos;

        if (line == 1) {
            lineStartPos = srcFile.hasUTF8BOM() ? 3 : 0;
        } else {
            lineStartPos = srcFile.getNewLineChars().get(line - 2) + 1;
        }

        long l = lineStartPos;
        while (l < errStart) {
            MultiByteChar mbChar = srcFile.getMultiByteChar(l);
            if (Objects.nonNull(mbChar)) {
                l += mbChar.getLength();

                // Some unicode codepoints are more than 1 column wide.
                int codepointWidth = UnicodeDisplay.getWidth(mbChar.getCodepoint());
                for (int i = 0; i < codepointWidth; i++) {
                    builder.append(' ');
                }

            } else {
                l++;
                builder.append(' ');
            }
        }

        return builder.toString();
    }

    /**
     * This method gets the space padding required at the of the line.
     *
     * @param size The length of the offending text
     * @return The String containing the line padding
     */
    private static String getOffendingTextMarker(long startPos, int size, SourceFile srcFile) {
        StringBuilder builder = new StringBuilder();

        long errEnd = startPos + size;
        long l = startPos;
        while (l < errEnd) {
            MultiByteChar mbChar = srcFile.getMultiByteChar(startPos);
            if (Objects.nonNull(mbChar)) {
                l += mbChar.getLength();

                // Some unicode codepoints are more than 1 column wide.
                int codepointWidth = UnicodeDisplay.getWidth(mbChar.getCodepoint());
                for (int i = 0; i < codepointWidth; i++) {
                    builder.append('^');
                }

            } else {
                l++;
                builder.append('^');
            }
        }

        return builder.toString();
    }

    /**
     * This method returns the padding string consisting of spaces with the total digits.
     *
     * @param digitsInLine The total number of digits in the line Number
     * @return The padding string with the total number of digits.
     */
    private static String getLineNumberPadding(int digitsInLine) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < digitsInLine; i++) {
            builder.append(' ');
        }

        return builder.toString();
    }

    /**
     * This method returns the number of digits in a specific number.
     *
     * @param lineNumber The line number of the diagnosed error
     * @return The number of digits in the line number.
     */
    private static int getTotalDigitsInNumber(int lineNumber) {
        // Since the line number will never be 0, logarithmic functions work fine.
        return (int) (Math.log10(lineNumber) + 1);
    }

    // This array is a lookup table for each of the possible error diagnosis messages
    private static final String[] errorDiagnosisMessages = {
            "a source file cannot end with an unclosed multi-line comment",
            "the bitwise operator '&' is not part of the AP subset",
            "the bitwise operator '|' is not part of the AP subset",
            "expected digit after underscore separator in numeric literal",
            "numeric literals of type 'long' are not part of the AP subset",
            "decimal literals of type 'float' are not part of the AP subset",
            "numeric underscore separators are not allowed after the decimal point",
            "expected digit after exponent indicator in double literal",
            "expected hexadecimal digit after '0x' specifier",
            "expected hex digit after floating point in hexadecimal double literal",
            "expected exponent part in hexadecimal double literal",
            "hexadecimal (base 16) numeric literals are not part of the AP subset",
            "binary (base 2) numeric literals are not part of the AP subset",
            "octal (base 8) numeric literals are not part of the AP subset",
            "expected floating point and/or fractional part in a double literal beginning with '0'",
            "characters represented by a literal of type 'char' must be less than 0xffff",
            "expected closing single quote to end 'char' literal",
            "literals of type 'char' are not part of the AP subset",
            "this escape sequence is not part of the AP subset",
            "invalid escape sequence",
            "expected 4 hexadecimal digits after 'u' in unicode escape sequence",
            "unexpected file end within string literal",
            "newline characters are not allowed in string literals",
            "unexpected character"
    };

    // This array is a lookup table for each of the possible error help messages
    private static final String[] errorHelpMessages = {
            "consider adding '*/' at the end of the file",
            "did you mean '&&' instead?",
            "did you mean '||' instead?",
            "consider adding a digit here",
            "consider changing it to an 'int' literal by removing the 'l' or 'L' suffix",
            "consider changing it to a 'double' literal by removing the 'd' or 'D' suffix",
            "consider removing the underscore",
            "consider adding a digit here",
            "consider adding a hex digit (0-9, a-f) here",
            "consider adding a hex digit (0-9, a-f) here",
            "consider adding the exponent part beginning with 'p' here",
            "consider converting it to a decimal (base 10) literal",
            "consider converting it to a decimal (base 10) literal",
            "consider converting it to a decimal (base 10) literal",
            "consider removing the 0 at the start or adding the fractional part",
            "use an ASCII table to make sure any characters in a 'char' literal are less than 0xffff. Otherwise, use a String literal",
            "consider inserting \"'\" here",
            "represent all characters with a String literal instead",
            "remove it or replace it with '\\n', '\\\\', or '\\\"'",
            "remove it or replace it with '\\n', '\\\\', or '\\\"'",
            "add 4 digits, or right-justify it with 0s if the required number has 3 or less digits",
            "consider ending the string literal by adding '\"' to the end",
            "if you need a newline character, use '\\n' instead",
            "this character is not valid Java syntax. However, it can be wrapped in a String if you wish to print it."
    };
}
