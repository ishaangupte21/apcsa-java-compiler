package com.ishaan.apcsa_compiler.source;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * This object represents a single source file that has been opened and buffered into the source map.
 */
public class SourceFile {
    private final String path;

    private final ByteBuffer buffer;

    private final long offset;

    private final int size;

    private final boolean hasUTF8BOM;

    private final List<Long> newLineChars;

    private final Map<Long, MultiByteChar> multiByteChars;

    public SourceFile(String path, ByteBuffer buffer, long offset, int size, boolean hasUTF8BOM, List<Long> newLineChars, Map<Long, MultiByteChar> multiByteChars) {
        this.path = path;
        this.buffer = buffer;
        this.offset = offset;
        this.size = size;
        this.hasUTF8BOM = hasUTF8BOM;
        this.newLineChars = newLineChars;
        this.multiByteChars = multiByteChars;
    }

    public String getPath() {
        return path;
    }

    public ByteBuffer getBuffer() {
        return buffer;
    }

    public long getOffset() {
        return offset;
    }

    public int getSize() {
        return size;
    }

    public boolean hasUTF8BOM() {
        return hasUTF8BOM;
    }

    public List<Long> getNewLineChars() {
        return newLineChars;
    }

    public Map<Long, MultiByteChar> getMultiByteChars() {
        return multiByteChars;
    }

    /**
     * This method computes the position within the source file given a position local to this file.
     *
     * @param localPosition The local position within this source file.
     * @return A {@link SourcePosition} instance containing the file path, the line, and column of the location.
     */
    public SourcePosition getSourcePosition(long localPosition) {
        int line = getSourcePosLine(localPosition);
        int col = getSourcePosColumn(localPosition, line);

        return new SourcePosition(path, line, col);
    }

    /**
     * This method computes the line of a given location within the source file.
     *
     * @param localPosition The local position within the source file.
     * @return The line number of the given position.
     */
    private int getSourcePosLine(long localPosition) {
        // If we have no newline markers, we know that this is line 1.
        if (newLineChars.isEmpty())
            return 1;

        // We need to iterate through the newline markers until we find the closest one to the position.
        int newLineCharsLen = newLineChars.size();
        for (int i = 0; i < newLineCharsLen; i++) {
            // If the current newline character has a position greater than the localPosition, the index higher will be the line number.
            // For example, if the 2nd new-line character (at index 1) is greater than localPosition, we know that this is the 2nd line.
            if (newLineChars.get(i) > localPosition)
                return i + 1;
        }

        // If the local position is greater than all the newline positions, it is on the last line.
        // The total number of lines is one more than the number of newline characters.
        return newLineCharsLen + 1;
    }

    /**
     * This method computes the column of a given location within the source file.
     *
     * @param localPosition The local position within the source file.
     * @param line          The line number of the given position.
     * @return The column number of the given position.
     */
    private int getSourcePosColumn(long localPosition, int line) {
        long lineStartPos;
        if (line == 1) {
            // If the line is 1, we need to set the start position to 0 or 3, depending on the BOM.
            lineStartPos = hasUTF8BOM ? 3 : 0;
        } else {
            // Since the line number is one more than its position, we can get the local position by taking the index at line - 1.
            // As we want the start position of the line, we will increment the newLine position by 1.
            lineStartPos = newLineChars.get(line - 2) + 1;
        }

        int col = 1;

        // We will begin at the newline position and move up by character until we hit the localPosition.
        while (lineStartPos < localPosition) {
            MultiByteChar mbChar = multiByteChars.get(lineStartPos);
            // If we have a multibyte character at this position, we need to move up by the length of that character.
            if (Objects.nonNull(mbChar)) {
                lineStartPos += mbChar.getLength();
            } else {
                // Otherwise, we move up by 1.
                lineStartPos++;
            }

            // For each character, we move the column up by 1.
            col++;
        }

        return col;
    }

    /**
     * This method queries the multiByteChars Hash table in order to check if there is a multibyte character at the given position.
     *
     * @param localPosition The local position within the file to be checked.
     * @return The response from calling get() on the HashMap.
     */
    public MultiByteChar getMultiByteChar(long localPosition) {
        return multiByteChars.get(localPosition);
    }

    /**
     * This method gets the text of a specific line from this source file.
     *
     * @param line The line number for which the text will be retrieved.
     * @return A string consisting of the text content of the line.
     */
    public String getLineText(int line) {
        // Since we have the line number, we can find the start position of the line.
        long lineStartPos;
        if (line == 1) {
            lineStartPos = hasUTF8BOM ? 3 : 0;
        } else {
            lineStartPos = newLineChars.get(line - 2) + 1;
        }

        // This cast is safe as a single file size can never be more than Integer.MAX_VALUE.
        int intLineStartPos = (int) lineStartPos;
        int lineEndPos;

        // Now, we need to find the end position of the line.
        int totalLines = newLineChars.size() + 1;
        // If we have the last line, the end of the file is the end of the line.
        if (line == totalLines) {
            lineEndPos = size;
        } else {
            // Otherwise, we need to find the end of the current line.
            lineEndPos = newLineChars.get(line - 1).intValue();
        }

        int lineLength = lineEndPos - intLineStartPos;
        byte[] tempByteBuffer = new byte[lineLength];

        // Now, we need to move through the ByteBuffer from the lineStartPos and add the file characters to the buffer.
        for (int i = 0; i < lineLength; i++) {
            tempByteBuffer[i] = buffer.get(intLineStartPos + i);
        }

        return new String(tempByteBuffer, StandardCharsets.UTF_8);
    }

    /**
     * This file obtains the text of a specific length from a certain starting point.
     *
     * @param relativePosition The starting position of the token within the file
     * @param size             The length of the token
     * @return The String contents of the token
     */
    public String getText(long relativePosition, int size) {
        byte[] tempBuffer = new byte[size];

        int index = (int) relativePosition;
        for (int i = 0; i < size; i++) {
            tempBuffer[i] = buffer.get(index++);
        }

        return new String(tempBuffer, StandardCharsets.UTF_8);
    }
}