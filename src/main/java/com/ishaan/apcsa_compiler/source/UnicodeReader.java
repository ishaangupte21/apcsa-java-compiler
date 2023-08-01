package com.ishaan.apcsa_compiler.source;

import java.nio.ByteBuffer;

/**
 * This class serves as an interface for decoding UTF-8 encoded multibyte unicode characters.
 * It has one accessible static method which will be called for decoding.
 */
public class UnicodeReader {

    /**
     * This method reads a UTF-8 encoded Unicode codepoint from the file buffer.
     *
     * @param leadByte   The first byte of the codepoint, obtained while checking for ASCII characters.
     * @param currPos    The current position within the file buffer.
     * @param buffer     The {@link ByteBuffer} instance containing the buffered file contents.
     * @param bufferSize The size of the file buffer.
     * @return A {@link MultiByteChar} instance with the decoded codepoint value and the length.
     * @throws UnicodeReadingException When an invalid Unicode lead byte is encountered.
     */
    public static MultiByteChar decodeCodepoint(byte leadByte, int currPos, ByteBuffer buffer, int bufferSize) throws UnicodeReadingException {
        // In this case, we will never have a value below 0x80 as those have already been handled.
        int unsignedLeadByte = byteToUnsignedInt(leadByte);

        // A character with 2 bytes.
        if ((unsignedLeadByte & 0xe0) == 0xc0) {
            if (++currPos >= bufferSize)
                throw UnicodeReadingException.INSTANCE;

            int secondUnsignedByte = byteToUnsignedInt(buffer.get(currPos));
            int cp = ((unsignedLeadByte & 0x1f) << 6) | ((secondUnsignedByte & 0x3f));

            return new MultiByteChar(cp, 2);
        }

        // A character with 3 bytes.
        if ((unsignedLeadByte & 0xf0) == 0xe0) {
            if (currPos + 2 >= bufferSize)
                throw UnicodeReadingException.INSTANCE;

            int secondUnsignedByte = byteToUnsignedInt(buffer.get(currPos + 1));
            int thirdUnsignedByte = byteToUnsignedInt(buffer.get(currPos + 2));
            int cp = ((unsignedLeadByte & 0x0f) << 12) | ((secondUnsignedByte & 0x3f) << 6) | ((thirdUnsignedByte & 0x3f));

            return new MultiByteChar(cp, 3);
        }

        // A character with 4 bytes.
        if ((unsignedLeadByte & 0xf8) == 0xf0 && (unsignedLeadByte < 0xf4)) {
            if (currPos + 3 >= bufferSize)
                throw UnicodeReadingException.INSTANCE;

            int secondUnsignedByte = byteToUnsignedInt(buffer.get(currPos + 1));
            int thirdUnsignedByte = byteToUnsignedInt(buffer.get(currPos + 2));
            int fourthUnsignedByte = byteToUnsignedInt(buffer.get(currPos + 3));
            int cp = ((unsignedLeadByte & 0x07) << 18) | ((secondUnsignedByte & 0x3f) << 12) | ((thirdUnsignedByte & 0x3f) << 6) | (fourthUnsignedByte & 0x3f);

            return new MultiByteChar(cp, 4);
        }

        // If the lead byte does not concur with any of these, we must throw a UnicodeReadingException
        throw UnicodeReadingException.INSTANCE;
    }

    /**
     * This method takes a signed two's complement byte and converts it to an unsigned int.
     *
     * @param b The byte value to be converted.
     * @return The unsigned value of the byte as an int.
     */
    private static int byteToUnsignedInt(byte b) {
        return ((int) b) & 0xff;
    }
}
