package com.ishaan.apcsa_compiler.source;

import com.ishaan.apcsa_compiler.util.Pair;

import java.nio.ByteBuffer;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Paths;
import java.util.*;

/**
 * This object implements the Source Map which will hold each source file buffer.
 * It is inspired by the Rust compiler.
 */
public class SourceMap {

    private static final int MAP_SIZE = 4 * 4096;

    private static final List<SourceFile> srcFiles = new ArrayList<>();

    // This contains the endpoint of the last read file to begin the next.
    private static long currentOffset = 0;

    // This byte array represents the UTF-8 BOM.
    private static final byte[] UTF8_BOM = {(byte) 0xef, (byte) 0xbb, (byte) 0xbf};

    /**
     * This method will read a new source file and buffer it into the source map.
     *
     * @param path The path of the source file to read.
     * @return The {@link SourceFile} instance created from the given path.
     */
    public static SourceFile bufferSrcFile(String path) throws Exception {
        // First, we need to open the source file and get its contents into a buffer.
        Pair<ByteBuffer, Integer> sourceFileData = readSrcFile(path);

        // Now, we need to get the source file object by analyzing the buffer.
        SourceFile createdSourceFile = analyzeSrcFile(sourceFileData.getFirst(), sourceFileData.getSecond(), path);

        // Add the source file.
        srcFiles.add(createdSourceFile);

        return createdSourceFile;
    }

    /**
     * This method will read the source file and buffer it into the source map.
     *
     * @param path The source file path as a {@link String}.
     * @return A tuple {@link Pair} with the file {@link ByteBuffer} and its size.
     * @throws SourceFileTooLargeException if the file size is larger than 32 bits.
     */
    private static Pair<ByteBuffer, Integer> readSrcFile(String path) throws Exception {
        try (FileChannel fileChannel = FileChannel.open(Paths.get(path))) {

            // Since source files given to this compiler will be relatively small, we will only support file sizes upto 32 bits.
            if (fileChannel.size() > Integer.MAX_VALUE)
                throw SourceFileTooLargeException.INSTANCE;

            int fileSize = (int) fileChannel.size();

            // If a source file is larger than 4 * 4096 bytes, we will map it.
            // Otherwise, we will use a ByteBuffer to avoid fragmenting the address space.
            if (fileSize > MAP_SIZE) {
                MappedByteBuffer mappedFile = fileChannel.map(FileChannel.MapMode.READ_ONLY, 0, fileSize);

                // Now, we can return the mapped byte buffer with the file size.
                return new Pair<>(mappedFile, fileSize);
            }

            // If the file is smaller, we will read into a ByteBuffer allocated with the file size.
            ByteBuffer fileBuffer = ByteBuffer.allocate(fileSize);
            fileChannel.read(fileBuffer);

            return new Pair<>(fileBuffer, fileSize);
        }
    }

    /**
     * This method iterates through the source file buffer, plots the location of each newline character, decodes each multibyte UTF-8 character and stores it.
     *
     * @param fileBuffer The {@link ByteBuffer} instance containing the buffered file contents.
     * @param fileSize   The length of the file in bytes.
     * @param path       The file's path as a String;
     * @return A {@link SourceFile} instance containing all of the relevant file metadata.
     */
    private static SourceFile analyzeSrcFile(ByteBuffer fileBuffer, Integer fileSize, String path) throws UnicodeReadingException {

        List<Long> newLineChars = new ArrayList<>();
        Map<Long, MultiByteChar> multiByteChars = new HashMap<>();

        int pos = 0;

        // Since reading to a ByteBuffer moves its pointer to the end, we need to rewind it.
        fileBuffer.rewind();

        // First, we need to check for the UTF-8 BOM.
        boolean hasUTF8BOM = false;
        if (fileSize >= 3) {
            byte[] firstThreeBuffer = new byte[3];
            fileBuffer.get(firstThreeBuffer, 0, 3);
            if (Arrays.equals(firstThreeBuffer, UTF8_BOM)) {
                pos = 3;
                hasUTF8BOM = true;
            }
        }

        // Here, we must iterate through the buffer to find all new-line characters and multibyte unicode codepoints.
        while (pos < fileSize) {
            byte currByte = fileBuffer.get(pos);

            // Check for newline characters
            if (currByte == 0xa) {
                newLineChars.add(pos + currentOffset);
                pos++;
                continue;
            }

            if (currByte == 0xc) {
                // Since Java allows \r\n to be a single new-line character, we must insert both into the map.
                newLineChars.add(pos + currentOffset);

                if (fileBuffer.get(++pos) == 0xa) {
                    newLineChars.add(pos + currentOffset);
                    pos++;
                }

                continue;
            }

            // Now, we must check for multibyte characters.
            // If the value of the byte is 0 or positive, we have an ASCII character and can continue.
            if (currByte >= 0) {
                pos++;
                continue;
            }

            // Otherwise, we need to parse the UTF-8 unicode codepoints.
            long codepointStartPos = currentOffset + pos;

            MultiByteChar mbChar = UnicodeReader.decodeCodepoint(currByte, pos, fileBuffer, fileSize);
            multiByteChars.put(codepointStartPos, mbChar);

            pos += mbChar.getLength();
        }

        // Return the new Source File object.
        SourceFile thisFile = new SourceFile(path, fileBuffer, currentOffset, fileSize, hasUTF8BOM, newLineChars, multiByteChars);
        currentOffset += fileSize;

        return thisFile;
    }

    public static SourcePosition getSourcePosition(long absolutePosition) {
        // First, we need to convert this absolute position into a local position.
        int totalSrcFiles = srcFiles.size();

        // If there is only 1 source file, we know that it is in that file.
        // Also, if there is only 1 source file, the absolute position will be the local position.
        if (totalSrcFiles == 1)
            return srcFiles.get(0).getSourcePosition(absolutePosition);

        // Otherwise, we need to iterate through each of the files until we find one with a start offset greater than the absolute position.
        for (int i = 1; i < totalSrcFiles; i++) {
            SourceFile currentSrcFile = srcFiles.get(i);
            if (currentSrcFile.getOffset() > absolutePosition) {
                // If the offset is greater, we know this position is part of the last file.
                SourceFile lastFile = srcFiles.get(i - 1);
                long localPosition = absolutePosition - lastFile.getOffset();

                // Now, we can use the source file and the local position to get the source location.
                return lastFile.getSourcePosition(localPosition);
            }
        }

        // If we get here, we must use the last file in the list.
        SourceFile finalFile = srcFiles.get(totalSrcFiles - 1);
        long localPosition = absolutePosition - finalFile.getOffset();

        return finalFile.getSourcePosition(localPosition);
    }
}
