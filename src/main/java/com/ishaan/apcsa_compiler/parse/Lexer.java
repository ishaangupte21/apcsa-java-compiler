package com.ishaan.apcsa_compiler.parse;

import com.ishaan.apcsa_compiler.errors.ErrorKind;
import com.ishaan.apcsa_compiler.errors.ErrorReporter;
import com.ishaan.apcsa_compiler.source.MultiByteChar;
import com.ishaan.apcsa_compiler.source.SourceFile;

import java.nio.ByteBuffer;
import java.util.Objects;

/**
 * This class is the Lexical Analyzer which will tokenize the Java input.
 */
public class Lexer {
    private final ByteBuffer fileBuffer;

    private final int fileSize;

    private final SourceFile srcFile;

    private final long srcFileOffset;

    private int pos = 0;

    private static final int EOF = -1;

    private int lastCpLength;

    public Lexer(SourceFile srcFile) {
        this.fileBuffer = srcFile.getBuffer();
        this.fileSize = srcFile.getSize();
        this.srcFile = srcFile;
        this.srcFileOffset = srcFile.getOffset();
    }

    /**
     * This method gets the next codepoint from the buffer.
     * It will check for any UTF-8 multibyte codepoints, and return them if they exist.
     *
     * @return The next codepoint from the input.
     */
    private int getNextCodepoint() {
        // If we are at the end of the buffer, return EOF.
        if (pos >= fileSize)
            return EOF;

        // Now, we need to check if this character is a multibyte character.
        MultiByteChar mbChar = srcFile.getMultiByteChar(pos);
        // If we have a multibyte character, return it.
        if (Objects.nonNull(mbChar)) {
            lastCpLength = mbChar.getLength();
            return mbChar.getCodepoint();
        }

        // Otherwise, return the byte value.
        lastCpLength = 1;
        return fileBuffer.get(pos);
    }

    /**
     * This method determines whether a given codepoint is not a hexadecimal digit.
     * <p>
     * IntelliJ suggested inverting the method when it was called isHexDigit...
     *
     * @param cp The codepoint to be checked.
     * @return false if the codepoint is a hexadecimal digit, otherwise true.
     */
    private static boolean isNotHexDigit(int cp) {
        switch (cp) {
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
            case 'a':
            case 'b':
            case 'c':
            case 'd':
            case 'e':
            case 'f':
            case 'A':
            case 'B':
            case 'C':
            case 'D':
            case 'E':
            case 'F':
                return false;
            default:
                return true;
        }
    }

    /**
     * This method determines if the given codepoint is not a binary digit.
     *
     * @param cp The codepoint to be checked
     * @return false if the codepoint is a binary digit, otherwise true.
     */
    private static boolean isNotBinaryDigit(int cp) {
        return cp != '0' && cp != '1';
    }

    /**
     * This method determines if the given codepoint is not an octal digit.
     *
     * @param cp The codepoint to be checked
     * @return false if the codepoint is a octal digit, otherwise true.
     */
    private static boolean isNotOctalDigit(int cp) {
        switch (cp) {
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
                return false;
            default:
                return true;
        }
    }

    /**
     * This method is the main lexer routine. It will tokenize the input using DFA based on the Java language specification.
     * In order to avoid cluttering the heap, the Parser will have one token instance which will be passed to the lexer and mutated by it.
     *
     * @param tok The {@link Token} instance to be mutated by the Lexer with the scanned token information.
     */
    public void getNextToken(Token tok) {
        // Since we need to be able to restart the lexer, we will wrap it in an infinite loop.
        while (true) {
            int cp = getNextCodepoint();

            // First, we need to consume all whitespace.
            while (Character.isWhitespace(cp)) {
                pos += lastCpLength;
                cp = getNextCodepoint();
            }

            long tokenRelativeStart = pos, tokenAbsoluteStart = pos + srcFileOffset;
            // Now, we can begin the main lexer token DFA.
            switch (cp) {
                case EOF:
                    tok.make(TokenKind.EOF, tokenAbsoluteStart, tokenRelativeStart, 1);
                    return;

                // Now, we can handle single-byte characters.
                case ';':
                    tok.make(TokenKind.SEMICOLON, tokenAbsoluteStart, tokenRelativeStart, 1);
                    pos++;
                    return;

                case '(':
                    tok.make(TokenKind.L_PAREN, tokenAbsoluteStart, tokenRelativeStart, 1);
                    pos++;
                    return;
                case ')':
                    tok.make(TokenKind.R_PAREN, tokenAbsoluteStart, tokenRelativeStart, 1);
                    pos++;
                    return;
                case '{':
                    tok.make(TokenKind.L_CURLY, tokenAbsoluteStart, tokenRelativeStart, 1);
                    pos++;
                    return;
                case '}':
                    tok.make(TokenKind.R_CURLY, tokenAbsoluteStart, tokenRelativeStart, 1);
                    pos++;
                    return;
                case '[':
                    tok.make(TokenKind.L_SQUARE, tokenAbsoluteStart, tokenRelativeStart, 1);
                    pos++;
                    return;
                case ']':
                    tok.make(TokenKind.R_SQUARE, tokenAbsoluteStart, tokenRelativeStart, 1);
                    pos++;
                    return;
                case ',':
                    tok.make(TokenKind.COMMA, tokenAbsoluteStart, tokenRelativeStart, 1);
                    pos++;
                    return;

                // Now, we will handle operators.
                case '+': {
                    pos++;

                    int nextCp = getNextCodepoint();
                    if (nextCp == '+') {
                        tok.make(TokenKind.PLUS_PLUS, tokenAbsoluteStart, tokenRelativeStart, 2);
                        pos++;
                        return;
                    }
                    if (nextCp == '=') {
                        tok.make(TokenKind.PLUS_EQUALS, tokenAbsoluteStart, tokenRelativeStart, 2);
                        pos++;
                        return;
                    }

                    tok.make(TokenKind.PLUS, tokenAbsoluteStart, tokenRelativeStart, 1);
                    return;
                }
                case '-': {
                    pos++;

                    int nextCp = getNextCodepoint();
                    if (nextCp == '-') {
                        tok.make(TokenKind.MINUS_MINUS, tokenAbsoluteStart, tokenRelativeStart, 2);
                        pos++;
                        return;
                    }
                    if (nextCp == '=') {
                        tok.make(TokenKind.MINUS_EQUALS, tokenAbsoluteStart, tokenRelativeStart, 2);
                        pos++;
                        return;
                    }

                    tok.make(TokenKind.MINUS, tokenAbsoluteStart, tokenRelativeStart, 1);
                    return;
                }
                case '*': {
                    pos++;

                    if (getNextCodepoint() == '=') {
                        tok.make(TokenKind.STAR_EQUALS, tokenAbsoluteStart, tokenRelativeStart, 2);
                        pos++;
                        return;
                    }

                    tok.make(TokenKind.STAR, tokenAbsoluteStart, tokenRelativeStart, 1);
                    return;
                }
                case '%': {
                    pos++;

                    if (getNextCodepoint() == '=') {
                        tok.make(TokenKind.PERCENT_EQUALS, tokenAbsoluteStart, tokenRelativeStart, 2);
                        pos++;
                        return;
                    }

                    tok.make(TokenKind.PERCENT, tokenAbsoluteStart, tokenRelativeStart, 1);
                    return;
                }
                case '/': {
                    pos++;
                    switch (getNextCodepoint()) {
                        case '/':
                            // Single line comments
                            // This method will return true to keep lexing and false to return.
                            if (lexSingleLineComment(tok))
                                continue;

                            return;
                        case '*':
                            // Multi line comments
                            // This method will return true to keep lexing and false to return.
                            if (lexMultiLineComment(tok))
                                continue;

                            return;
                        case '=':
                            tok.make(TokenKind.SLASH_EQUALS, tokenAbsoluteStart, tokenRelativeStart, 2);
                            pos++;
                            return;
                        default:
                            tok.make(TokenKind.SLASH, tokenAbsoluteStart, tokenRelativeStart, 1);
                            return;
                    }
                }
                case '=': {
                    pos++;

                    if (getNextCodepoint() == '=') {
                        tok.make(TokenKind.EQUALS_EQUALS, tokenAbsoluteStart, tokenRelativeStart, 2);
                        pos++;
                        return;
                    }

                    tok.make(TokenKind.EQUALS, tokenAbsoluteStart, tokenRelativeStart, 1);
                    return;
                }
                case '!': {
                    pos++;

                    if (getNextCodepoint() == '=') {
                        tok.make(TokenKind.EXCLAMATION_EQUALS, tokenAbsoluteStart, tokenRelativeStart, 2);
                        pos++;
                        return;
                    }

                    tok.make(TokenKind.EXCLAMATION, tokenAbsoluteStart, tokenRelativeStart, 1);
                    return;
                }
                case '>': {
                    pos++;

                    if (getNextCodepoint() == '=') {
                        tok.make(TokenKind.GREATER_EQUALS, tokenAbsoluteStart, tokenRelativeStart, 2);
                        pos++;
                        return;
                    }

                    tok.make(TokenKind.GREATER, tokenAbsoluteStart, tokenRelativeStart, 1);
                    return;
                }
                case '<': {
                    pos++;

                    if (getNextCodepoint() == '=') {
                        tok.make(TokenKind.LESS_EQUALS, tokenAbsoluteStart, tokenRelativeStart, 2);
                        pos++;
                        return;
                    }

                    tok.make(TokenKind.LESS, tokenAbsoluteStart, tokenRelativeStart, 1);
                    return;
                }

                // In the CollegeBoard AP subset, '&&' and '||' are operators, but not '&' and '|'.
                case '&': {
                    pos++;
                    if (getNextCodepoint() == '&') {
                        tok.make(TokenKind.AMPERSAND_AMPERSAND, tokenAbsoluteStart, tokenRelativeStart, 2);
                        pos++;
                        return;
                    }

                    // Diagnose the error.
                    ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_OPERATOR_AMPERSAND, tokenRelativeStart, 1, srcFile);
                    // For error recovery, we will temporarily return the '&&' token.
                    tok.make(TokenKind.AMPERSAND_AMPERSAND, tokenAbsoluteStart, tokenRelativeStart, 1);
                    return;
                }
                case '|': {
                    pos++;
                    if (getNextCodepoint() == '|') {
                        tok.make(TokenKind.BAR_BAR, tokenAbsoluteStart, tokenRelativeStart, 2);
                        pos++;
                        return;
                    }

                    // Diagnose the error.
                    ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_OPERATOR_BAR, tokenRelativeStart, 1, srcFile);
                    // For error recovery, we will temporarily return the '||' token.
                    tok.make(TokenKind.BAR_BAR, tokenAbsoluteStart, tokenRelativeStart, 1);
                    return;
                }

                case '.': {
                    // If the dot is followed by a digit, we must proceed to scan a double literal.
                    int nextPos = pos + 1;
                    if (nextPos < fileSize && Character.isDigit(fileBuffer.get(nextPos))) {
                        lexDoubleLiteral(tok, tokenRelativeStart, tokenAbsoluteStart);
                        return;
                    }

                    // Otherwise, we can return a single character dot token.
                    pos++;
                    tok.make(TokenKind.DOT, tokenAbsoluteStart, tokenRelativeStart, 1);
                    return;
                }

                // Now, we will begin scanning literals.
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    lexNumericLiteral(tok, tokenRelativeStart, tokenAbsoluteStart);
                    return;

                case '0': {
                    // Consume the 0.
                    pos++;

                    switch (getNextCodepoint()) {
                        // Hex literals
                        case 'x':
                        case 'X':
                            lexHexNumericLiteral(tok, tokenRelativeStart, tokenAbsoluteStart);
                            return;

                        // Binary literals
                        case 'b':
                        case 'B':
                            lexBinaryNumericLiteral(tok, tokenRelativeStart, tokenAbsoluteStart);
                            return;

                        case '1':
                        case '2':
                        case '3':
                        case '4':
                        case '5':
                        case '6':
                        case '7':
                            lexOctalNumericLiteral(tok, tokenRelativeStart, tokenAbsoluteStart);
                            return;

                        default: {
                            // For any other characters we can return the 0 literal token.
                            tok.make(TokenKind.ZERO_LITERAL, tokenAbsoluteStart, tokenRelativeStart, 1);
                            return;
                        }
                    }
                }
            }
        }
    }

    /**
     * This method scans single line comments from the input.
     *
     * @param tok the {@link Token} instance which is to be altered if the end of the file is found.
     * @return true if the end of the file is not found and false if it is not.
     */
    private boolean lexSingleLineComment(Token tok) {
        // Consume the 2nd slash.
        pos++;

        // Now, we need to consume all codepoints while we don't get an EOF or newline character.
        while (true) {
            switch (getNextCodepoint()) {
                case '\n':
                    // Consume the line terminator.
                    pos++;
                    return true;

                case '\r':
                    if (pos < fileSize && fileBuffer.get(++pos) == '\n') {
                        pos++;
                    }
                    return true;

                case EOF:
                    tok.make(TokenKind.EOF, pos + srcFileOffset, pos, 1);
                    return false;

                default:
                    pos += lastCpLength;
                    break;
            }
        }
    }

    /**
     * This method scans multi line comments from the input.
     *
     * @param tok the {@link Token} instance which is to be altered if the end of the file is found.
     * @return true if the end of the file is not found and false if it is not.
     */
    private boolean lexMultiLineComment(Token tok) {
        // Consume the star
        pos++;

        while (true) {
            int cp = getNextCodepoint();

            if (cp == '*') {
                // If we have the end delimiter, consume the slash and return true to keep lexing.
                if (pos < fileSize && fileBuffer.get(++pos) == '/') {
                    pos++;
                    return true;
                }

                continue;
            }

            if (cp == EOF) {
                // EOF in a multi-line comment is an error.
                ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_EOF_MULTILINE_COMMENT, pos, 1, srcFile);
                tok.make(TokenKind.EOF, pos + srcFileOffset, pos, 1);
                return false;
            }

            pos += lastCpLength;
        }
    }

    /**
     * This method will scan numeric literals from the input.
     * It will begin by scanning digits for an integer literal, but if a floating point is encountered, it will switch to scanning double literals.
     *
     * @param tok                The {@link Token} instance which will be mutated with the new token information.
     * @param tokenRelativeStart The starting position of the token within the source file.
     * @param tokenAbsoluteStart The absolute starting position of the token in the source map.
     */
    private void lexNumericLiteral(Token tok, long tokenRelativeStart, long tokenAbsoluteStart) {
        // Consume the starting numeric digit.
        pos++;

        // Since digits can be separated by numeric separators (underscore), we need to consume them in chunks.
        while (true) {
            switch (getNextCodepoint()) {
                // We need to consume all digits.
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    pos++;
                    continue;

                    // Numeric separator
                case '_': {
                    int posBeforeSeparator = pos;

                    // A numeric separator can consist of multiple underscores, so we need to consume them all.
                    int nextCodepoint;
                    do {
                        pos++;
                        nextCodepoint = getNextCodepoint();
                    } while (nextCodepoint == '_');

                    // If there are no more underscores, the next codepoint must be a digit.
                    if (!Character.isDigit(nextCodepoint)) {
                        ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_CHAR_AFTER_NUMERIC_SEPARATOR, pos, 1, srcFile);
                        // For error recovery, we will return an integer literal upto the digit before the separator.
                        int tokenSize = (int) (posBeforeSeparator - tokenRelativeStart);
                        tok.make(TokenKind.INT_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                        return;
                    }

                    // If we have a digit, we can consume it and keep going.
                    pos++;
                    continue;
                }

                // Long literal suffix
                case 'L':
                case 'l': {
                    // Long literals are not part of the AP subset, so we must report the error.
                    pos++;
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_LONG_LITERAL, tokenRelativeStart, tokenSize, srcFile);
                    tok.make(TokenKind.LONG_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }

                // Double literal suffix
                case 'd':
                case 'D': {
                    pos++;
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    tok.make(TokenKind.DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }

                // Float literal suffix - not part of the AP subset.
                case 'f':
                case 'F': {
                    pos++;
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_FLOAT_LITERAL, tokenRelativeStart, tokenSize, srcFile);
                    tok.make(TokenKind.FLOAT_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }

                // Floating point
                case '.':
                    lexDoubleLiteral(tok, tokenRelativeStart, tokenAbsoluteStart);
                    return;

                // Exponent
                case 'e':
                case 'E':
                    lexDoubleLiteralExponentPart(tok, tokenRelativeStart, tokenAbsoluteStart);
                    return;

                default:
                    break;
            }


            // If we get none of these, we have the end of the integer literal.
            int tokenSize = (int) (pos - tokenRelativeStart);
            tok.make(TokenKind.INT_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
            return;
        }
    }

    /**
     * This method will scan the fractional part of double literals from the input.
     * If the exponent indicator is encountered, it switches to scanning the exponent part
     *
     * @param tok                The {@link Token} instance which will be mutated with the new token information.
     * @param tokenRelativeStart The starting position of the token within the source file.
     * @param tokenAbsoluteStart The absolute starting position of the token in the source map.
     */
    private void lexDoubleLiteral(Token tok, long tokenRelativeStart, long tokenAbsoluteStart) {
        // Consume the floating point.
        pos++;

        // We cannot have a numeric separator after the floating point.
        int nextCodepoint = getNextCodepoint();
        if (nextCodepoint == '_') {
            ErrorReporter.reportWithLocalFilePosition(ErrorKind.NUMERIC_SEPARATOR_AFTER_FLOATING_POINT, pos, 1, srcFile);
            int tokenSize = (int) (pos - tokenRelativeStart);
            tok.make(TokenKind.DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
            return;
        }

        while (true) {
            // Now, we need to consume digits and separators again.
            switch (nextCodepoint) {
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    pos++;
                    nextCodepoint = getNextCodepoint();
                    continue;

                    // Numeric separator
                case '_': {
                    int posBeforeSeparator = pos;

                    // A numeric separator can consist of multiple underscores, so we need to consume them all.
                    int cpAfterSeparator;
                    do {
                        pos++;
                        cpAfterSeparator = getNextCodepoint();
                    } while (cpAfterSeparator == '_');

                    // If there are no more underscores, the next codepoint must be a digit.
                    if (!Character.isDigit(cpAfterSeparator)) {
                        ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_CHAR_AFTER_NUMERIC_SEPARATOR, pos, 1, srcFile);
                        // For error recovery, we will return a double literal upto the digit before the separator.
                        int tokenSize = (int) (posBeforeSeparator - tokenRelativeStart);
                        tok.make(TokenKind.DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                        return;
                    }

                    // If we have a digit, we can consume it and keep going.
                    pos++;
                    nextCodepoint = getNextCodepoint();
                    continue;
                }

                // Double literal suffix
                case 'd':
                case 'D': {
                    pos++;
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    tok.make(TokenKind.DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }

                // Float literal suffix - not part of the AP subset.
                case 'f':
                case 'F': {
                    pos++;
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_FLOAT_LITERAL, tokenRelativeStart, tokenSize, srcFile);
                    tok.make(TokenKind.FLOAT_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }

                // Exponent indicator
                case 'e':
                case 'E':
                    lexDoubleLiteralExponentPart(tok, tokenRelativeStart, tokenAbsoluteStart);
                    return;

                default:
                    break;
            }

            // If we get no other numeric characters, we can end the literal.
            int tokenSize = (int) (pos - tokenRelativeStart);
            tok.make(TokenKind.DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
            return;
        }
    }

    /**
     * This method will scan the exponent part of double literals from the input.
     *
     * @param tok                The {@link Token} instance which will be mutated with the new token information.
     * @param tokenRelativeStart The starting position of the token within the source file.
     * @param tokenAbsoluteStart The absolute starting position of the token in the source map.
     */
    private void lexDoubleLiteralExponentPart(Token tok, long tokenRelativeStart, long tokenAbsoluteStart) {
        // Consume the exponent indicator
        pos++;

        // We can have a sign just after the exponent indicator.
        int signCodepoint = getNextCodepoint();
        boolean hasSeenSign = false;
        if (signCodepoint == '+' || signCodepoint == '-') {
            // Consume the sign if it exists.
            pos++;
            hasSeenSign = true;
        }

        // The codepoint following the exponent indicator or sign must be a digit.
        if (!Character.isDigit(getNextCodepoint())) {
            ErrorReporter.reportWithLocalFilePosition(ErrorKind.NO_DIGIT_AFTER_EXPONENT_INDICATOR, pos, 1, srcFile);
            // We will set the token until before the exponent.
            int tokenSize = hasSeenSign ? (int) (pos - 2 - tokenRelativeStart) : (int) (pos - 1 - tokenRelativeStart);
            tok.make(TokenKind.DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
            return;
        }

        // Consume the first digit, since we have verified that it exists.
        pos++;

        // Now, we must consume all digits and numeric separators.
        while (true) {
            switch (getNextCodepoint()) {
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    pos++;
                    continue;

                    // Numeric separator
                case '_': {
                    int posBeforeSeparator = pos;

                    // A numeric separator can consist of multiple underscores, so we need to consume them all.
                    int nextCodepoint;
                    do {
                        pos++;
                        nextCodepoint = getNextCodepoint();
                    } while (nextCodepoint == '_');

                    // If there are no more underscores, the next codepoint must be a digit.
                    if (!Character.isDigit(nextCodepoint)) {
                        ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_CHAR_AFTER_NUMERIC_SEPARATOR, pos, 1, srcFile);
                        // For error recovery, we will return a double literal upto the digit before the separator.
                        int tokenSize = (int) (posBeforeSeparator - tokenRelativeStart);
                        tok.make(TokenKind.DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                        return;
                    }

                    // If we have a digit, we can consume it and keep going.
                    pos++;
                    continue;
                }

                case 'd':
                case 'D': {
                    pos++;
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    tok.make(TokenKind.DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }

                // Float literal suffix - not part of the AP subset.
                case 'f':
                case 'F': {
                    pos++;
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_FLOAT_LITERAL, tokenRelativeStart, tokenSize, srcFile);
                    tok.make(TokenKind.FLOAT_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }

                default:
                    break;
            }

            // If we get no other numeric characters, we can end the literal.
            int tokenSize = (int) (pos - tokenRelativeStart);
            tok.make(TokenKind.DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
            return;
        }
    }

    /**
     * This method will scan hexadecimal numeric literals from the input.
     *
     * @param tok                The {@link Token} instance which will be mutated with the new token information.
     * @param tokenRelativeStart The starting position of the token within the source file.
     * @param tokenAbsoluteStart The absolute starting position of the token in the source map.
     */
    private void lexHexNumericLiteral(Token tok, long tokenRelativeStart, long tokenAbsoluteStart) {
        // Consume the 'x' or 'X'.
        pos++;

        // The character following the 'x' must be a hex digit.
        if (isNotHexDigit(getNextCodepoint())) {
            ErrorReporter.reportWithLocalFilePosition(ErrorKind.NO_HEX_DIGIT_AFTER_SPECIFIER, pos, 1, srcFile);
            // Since hex literals are outside the subset, they will never be parsed, so we don't need to return a dummy token.
            tok.make(TokenKind.HEX_DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, 2);
            return;
        }

        // Consume the first hex digit.
        pos++;

        while (true) {
            switch (getNextCodepoint()) {
                // Consume all hex digits.
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                case 'a':
                case 'b':
                case 'c':
                case 'd':
                case 'e':
                case 'f':
                case 'A':
                case 'B':
                case 'C':
                case 'D':
                case 'E':
                case 'F':
                    pos++;
                    continue;

                    // Numeric separator
                case '_': {
                    int posBeforeSeparator = pos;

                    // A numeric separator can consist of multiple underscores, so we need to consume them all.
                    int nextCodepoint;
                    do {
                        pos++;
                        nextCodepoint = getNextCodepoint();
                    } while (nextCodepoint == '_');

                    // If there are no more underscores, the next codepoint must be a digit.
                    if (isNotHexDigit(nextCodepoint)) {
                        ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_CHAR_AFTER_NUMERIC_SEPARATOR, pos, 1, srcFile);
                        // For error recovery, we will return an integer literal upto the digit before the separator.
                        int tokenSize = (int) (posBeforeSeparator - tokenRelativeStart);
                        tok.make(TokenKind.INT_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                        return;
                    }

                    // If we have a digit, we can consume it and keep going.
                    pos++;
                    continue;
                }

                // Long literal suffix
                case 'L':
                case 'l': {
                    // Long literals are not part of the AP subset, so we must report the error.
                    pos++;
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_LONG_LITERAL, tokenRelativeStart, tokenSize, srcFile);
                    tok.make(TokenKind.LONG_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }

                // Floating point
                case '.':
                    lexHexDoubleLiteral(tok, tokenRelativeStart, tokenAbsoluteStart);
                    return;

                default: {
                    // If there is no suffix, we can return the literal.
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_HEX_LITERAL, tokenRelativeStart, tokenSize, srcFile);
                    tok.make(TokenKind.HEX_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }
            }
        }
    }

    /**
     * This method scans the fractional portion of hexadecimal double literals.
     *
     * @param tok                The {@link Token} instance which will be mutated with the new token information.
     * @param tokenRelativeStart The starting position of the token within the source file.
     * @param tokenAbsoluteStart The absolute starting position of the token in the source map.
     */
    private void lexHexDoubleLiteral(Token tok, long tokenRelativeStart, long tokenAbsoluteStart) {
        // Consume the floating point.
        pos++;

        // Now, we must have hex digits.
        if (isNotHexDigit(getNextCodepoint())) {
            ErrorReporter.reportWithLocalFilePosition(ErrorKind.NO_HEX_DIGIT_AFTER_FLOATING_POINT, pos, 1, srcFile);
            // Since hex literals are outside the subset, they will never be parsed, so we don't need to return a dummy token.
            int tokenSize = (int) (pos - tokenRelativeStart);
            tok.make(TokenKind.HEX_DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
            return;
        }

        // We know that there is a hex digit.
        pos++;

        // Now, we must consume digits and numeric separators until we get the binary exponent indicator.
        while (true) {
            switch (getNextCodepoint()) {
                // Consume all hex digits.
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                case 'a':
                case 'b':
                case 'c':
                case 'd':
                case 'e':
                case 'f':
                case 'A':
                case 'B':
                case 'C':
                case 'D':
                case 'E':
                case 'F':
                    pos++;
                    continue;

                    // Numeric separator
                case '_': {
                    int posBeforeSeparator = pos;

                    // A numeric separator can consist of multiple underscores, so we need to consume them all.
                    int nextCodepoint;
                    do {
                        pos++;
                        nextCodepoint = getNextCodepoint();
                    } while (nextCodepoint == '_');

                    // If there are no more underscores, the next codepoint must be a digit.
                    if (isNotHexDigit(nextCodepoint)) {
                        ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_CHAR_AFTER_NUMERIC_SEPARATOR, pos, 1, srcFile);
                        // For error recovery, we will return an integer literal upto the digit before the separator.
                        int tokenSize = (int) (posBeforeSeparator - tokenRelativeStart);
                        tok.make(TokenKind.INT_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                        return;
                    }

                    // If we have a digit, we can consume it and keep going.
                    pos++;
                    continue;
                }

                // Exponent indicator
                case 'p':
                case 'P':
                    lexHexExponentLiteral(tok, tokenRelativeStart, tokenAbsoluteStart);
                    return;

                default: {
                    // For any other characters, we have an error because Java requires the exponent.
                    ErrorReporter.reportWithLocalFilePosition(ErrorKind.NO_HEX_DOUBLE_EXPONENT, pos, 1, srcFile);
                    // Since hex literals are outside the subset, they will never be parsed, so we don't need to return a dummy token.
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    tok.make(TokenKind.HEX_DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }
            }
        }
    }

    /**
     * This method scans the exponent part of hexadecimal double literals from the input.
     *
     * @param tok                The {@link Token} instance which will be mutated with the new token information.
     * @param tokenRelativeStart The starting position of the token within the source file.
     * @param tokenAbsoluteStart The absolute starting position of the token in the source map.
     */
    private void lexHexExponentLiteral(Token tok, long tokenRelativeStart, long tokenAbsoluteStart) {
        // Consume the exponent indicator.
        pos++;

        // Signs are allowed for the exponent part.
        int signCodepoint = getNextCodepoint();
        boolean hasSeenSign = false;
        if (signCodepoint == '+' || signCodepoint == '-') {
            // Consume the sign if it exists.
            pos++;
            hasSeenSign = true;
        }

        // The codepoint following the exponent indicator or sign must be a digit.
        if (!Character.isDigit(getNextCodepoint())) {
            ErrorReporter.reportWithLocalFilePosition(ErrorKind.NO_DIGIT_AFTER_EXPONENT_INDICATOR, pos, 1, srcFile);
            // We will set the token until before the exponent.
            int tokenSize = hasSeenSign ? (int) (pos - 2 - tokenRelativeStart) : (int) (pos - 1 - tokenRelativeStart);
            tok.make(TokenKind.DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
            return;
        }

        // Consume the digit.
        pos++;

        while (true) {
            switch (getNextCodepoint()) {
                // Consume all hex digits.
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    pos++;
                    continue;

                    // Numeric separator
                case '_': {
                    int posBeforeSeparator = pos;

                    // A numeric separator can consist of multiple underscores, so we need to consume them all.
                    int nextCodepoint;
                    do {
                        pos++;
                        nextCodepoint = getNextCodepoint();
                    } while (nextCodepoint == '_');

                    // If there are no more underscores, the next codepoint must be a digit.
                    if (isNotHexDigit(nextCodepoint)) {
                        ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_CHAR_AFTER_NUMERIC_SEPARATOR, pos, 1, srcFile);
                        // For error recovery, we will return an integer literal upto the digit before the separator.
                        int tokenSize = (int) (posBeforeSeparator - tokenRelativeStart);
                        tok.make(TokenKind.HEX_DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                        return;
                    }

                    // If we have a digit, we can consume it and keep going.
                    pos++;
                    continue;
                }

                // Float literal suffix - not part of the AP subset.
                case 'f':
                case 'F': {
                    pos++;
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_FLOAT_LITERAL, tokenRelativeStart, tokenSize, srcFile);
                    tok.make(TokenKind.HEX_DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }

                case 'd':
                case 'D': {
                    pos++;
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_HEX_LITERAL, tokenRelativeStart, tokenSize, srcFile);
                    tok.make(TokenKind.HEX_DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }
                default:
                    break;
            }

            // If we get none of these, we have the end of the integer literal.
            int tokenSize = (int) (pos - tokenRelativeStart);
            ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_HEX_LITERAL, tokenRelativeStart, tokenSize, srcFile);
            tok.make(TokenKind.HEX_DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
            return;
        }
    }

    /**
     * This method will scan binary numeric literals from the input.
     *
     * @param tok                The {@link Token} instance which will be mutated with the new token information.
     * @param tokenRelativeStart The starting position of the token within the source file.
     * @param tokenAbsoluteStart The absolute starting position of the token in the source map.
     */
    private void lexBinaryNumericLiteral(Token tok, long tokenRelativeStart, long tokenAbsoluteStart) {
        // Consume the 'b'.
        pos++;

        while (true) {
            switch (getNextCodepoint()) {
                // Consume all binary digits.
                case '0':
                case '1':
                    pos++;
                    continue;

                    // Numeric separator
                case '_': {
                    int posBeforeSeparator = pos;

                    // A numeric separator can consist of multiple underscores, so we need to consume them all.
                    int nextCodepoint;
                    do {
                        pos++;
                        nextCodepoint = getNextCodepoint();
                    } while (nextCodepoint == '_');

                    // If there are no more underscores, the next codepoint must be a digit.
                    if (isNotBinaryDigit(nextCodepoint)) {
                        ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_CHAR_AFTER_NUMERIC_SEPARATOR, pos, 1, srcFile);
                        // For error recovery, we will return an integer literal upto the digit before the separator.
                        int tokenSize = (int) (posBeforeSeparator - tokenRelativeStart);
                        tok.make(TokenKind.BINARY_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                        return;
                    }

                    // If we have a digit, we can consume it and keep going.
                    pos++;
                    continue;
                }

                // Long literal suffix
                case 'L':
                case 'l': {
                    // Long literals are not part of the AP subset, so we must report the error.
                    pos++;
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_LONG_LITERAL, tokenRelativeStart, tokenSize, srcFile);
                    tok.make(TokenKind.BINARY_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }

                default:
                    break;
            }

            int tokenSize = (int) (pos - tokenRelativeStart);
            ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_BINARY_LITERALS, tokenRelativeStart, tokenSize, srcFile);
            tok.make(TokenKind.BINARY_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
            return;
        }
    }

    /**
     * This method will scan octal numeric literals from the input.
     *
     * @param tok                The {@link Token} instance which will be mutated with the new token information.
     * @param tokenRelativeStart The starting position of the token within the source file.
     * @param tokenAbsoluteStart The absolute starting position of the token in the source map.
     */
    private void lexOctalNumericLiteral(Token tok, long tokenRelativeStart, long tokenAbsoluteStart) {
        // Consume the first non-zero octal digit.
        pos++;

        // Now, we must consume while we have octal digits.
        while (true) {
            switch (getNextCodepoint()) {
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                    pos++;
                    continue;

                    // Numeric separator
                case '_': {
                    int posBeforeSeparator = pos;

                    // A numeric separator can consist of multiple underscores, so we need to consume them all.
                    int nextCodepoint;
                    do {
                        pos++;
                        nextCodepoint = getNextCodepoint();
                    } while (nextCodepoint == '_');

                    // If there are no more underscores, the next codepoint must be a digit.
                    if (isNotOctalDigit(nextCodepoint)) {
                        ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_CHAR_AFTER_NUMERIC_SEPARATOR, pos, 1, srcFile);
                        // For error recovery, we will return an integer literal upto the digit before the separator.
                        int tokenSize = (int) (posBeforeSeparator - tokenRelativeStart);
                        tok.make(TokenKind.OCTAL_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                        return;
                    }

                    // If we have a digit, we can consume it and keep going.
                    pos++;
                    continue;
                }

                // If we get an 8 or 9, we need to convert to scanning a decimal double literal.
                case '8':
                case '9':
                    lexDoubleLiteralFromOctal(tok, tokenRelativeStart, tokenAbsoluteStart);
                    return;

                // Floating point - this is for decimal double literals
                case '.':
                    lexDoubleLiteral(tok, tokenRelativeStart, tokenAbsoluteStart);
                    return;


                // Long literal suffix
                case 'L':
                case 'l': {
                    // Long literals are not part of the AP subset, so we must report the error.
                    pos++;
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_LONG_LITERAL, tokenRelativeStart, tokenSize, srcFile);
                    tok.make(TokenKind.OCTAL_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }

                default:
                    break;
            }

            int tokenSize = (int) (pos - tokenRelativeStart);
            ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_OCTAL_LITERAL, tokenRelativeStart, tokenSize, srcFile);
            tok.make(TokenKind.OCTAL_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
            return;
        }
    }

    /**
     * This method will scan from an octal literal into a double literal.
     *
     * @param tok                The {@link Token} instance which will be mutated with the new token information.
     * @param tokenRelativeStart The starting position of the token within the source file.
     * @param tokenAbsoluteStart The absolute starting position of the token in the source map.
     */
    private void lexDoubleLiteralFromOctal(Token tok, long tokenRelativeStart, long tokenAbsoluteStart) {
        // Consume the first seen non-octal digit.
        pos++;

        while (true) {
            switch (getNextCodepoint()) {
                // Consume all digits.
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    pos++;
                    continue;

                    // Numeric separator
                case '_': {
                    int posBeforeSeparator = pos;

                    // A numeric separator can consist of multiple underscores, so we need to consume them all.
                    int nextCodepoint;
                    do {
                        pos++;
                        nextCodepoint = getNextCodepoint();
                    } while (nextCodepoint == '_');

                    // If there are no more underscores, the next codepoint must be a digit.
                    if (!Character.isDigit(nextCodepoint)) {
                        ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_CHAR_AFTER_NUMERIC_SEPARATOR, pos, 1, srcFile);
                        // For error recovery, we will return an integer literal upto the digit before the separator.
                        int tokenSize = (int) (posBeforeSeparator - tokenRelativeStart);
                        tok.make(TokenKind.DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                        return;
                    }

                    // If we have a digit, we can consume it and keep going.
                    pos++;
                    continue;
                }

                case '.':
                    lexDoubleLiteral(tok, tokenRelativeStart, tokenAbsoluteStart);
                    return;

                // Double literal suffix
                case 'd':
                case 'D': {
                    pos++;
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    tok.make(TokenKind.DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }

                // Float literal suffix - not part of the AP subset.
                case 'f':
                case 'F': {
                    pos++;
                    int tokenSize = (int) (pos - tokenRelativeStart);
                    ErrorReporter.reportWithLocalFilePosition(ErrorKind.ILLEGAL_FLOAT_LITERAL, tokenRelativeStart, tokenSize, srcFile);
                    tok.make(TokenKind.FLOAT_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
                    return;
                }

                // Exponent indicator
                case 'e':
                case 'E':
                    lexDoubleLiteralExponentPart(tok, tokenRelativeStart, tokenAbsoluteStart);
                    return;

                default:
                    break;
            }

            // Now, since we don't have the back part of the double literal, we need to diagnose an error.
            int tokenSize = (int) (pos - tokenRelativeStart);
            ErrorReporter.reportWithLocalFilePosition(ErrorKind.NO_FRACTION_PART_IN_OCTAL_TO_DOUBLE_LITERAL, tokenRelativeStart, tokenSize, srcFile);
            tok.make(TokenKind.DOUBLE_LITERAL, tokenAbsoluteStart, tokenRelativeStart, tokenSize);
            return;
        }
    }
}
