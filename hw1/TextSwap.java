/**
    Title  : CS 511 C - HW Assignment 1
    Desc   : Concurrent file writing
    Name   : Bobby Georgiou
    Date   : 05/15/2019
    Pledge : "I pledge my honor that I have abided by the Stevens Honor System."
 */
import java.io.*;
import java.util.*;

public class TextSwap {

    /**
     * Reads from specified file and returns the string of file content
     * 
     * @param filename the path of the file to read
     * @throws Exception
     * @return string of file content
     */
    private static String readFile(String filename) throws Exception {
        String line;
        StringBuilder buffer = new StringBuilder();
        File file = new File(filename);
        BufferedReader br = new BufferedReader(new FileReader(file));
        while ((line = br.readLine()) != null) {
            buffer.append(line);
        }
        br.close();
        return buffer.toString();
    }

    /**
     * Returns an array of Interval that specify the start and end index of each chunk
     * 
     * @param numChunks amount of chunks
     * @param chunkSize size of each chunk
     * @return array of Interval
     */
    private static Interval[] getIntervals(int numChunks, int chunkSize) {
        Interval[] intervals = new Interval[numChunks];
        for (int i = 0, c = 0; i < numChunks; i++, c += chunkSize) {
            Interval curInterval = new Interval(c, c + chunkSize - 1);
            intervals[i] = curInterval;
        }
        return intervals;
    }

    /**
     * Prompts for input of characters representing the order of chunks in the final buffer
     * 
     * @param numChunks amount of chunks
     * @return list of the labels
     */
    private static List<Character> getLabels(int numChunks) {
        Scanner scanner = new Scanner(System.in);
        List<Character> labels = new ArrayList<Character>();
        int endChar = numChunks == 0 ? 'a' : 'a' + numChunks - 1;
        System.out.printf("Input %d character(s) (\'%c\' - \'%c\') for the pattern.\n", numChunks, 'a', endChar);
        for (int i = 0; i < numChunks; i++) {
            labels.add(scanner.next().charAt(0));
        }
        scanner.close();
        return labels;
    }

    /**
     * Gets labels, creates and sorts intervals based on inputted labels, runs Swapper threads, returns the final buffer
     * 
     * @param content string of file content
     * @param chunkSize size of each chunk in the content
     * @param numChunks amount of chunks
     * @return the reordered buffer
     */
    private static char[] runSwapper(String content, int chunkSize, int numChunks) {
        List<Character> labels = getLabels(numChunks);
        Interval[] intervals = getIntervals(numChunks, chunkSize);
        // order the intervals properly, then run the Swapper instances
        Interval[] sortedIntervals = new Interval[numChunks];
        for (int i = 0; i < numChunks; i++) {
            sortedIntervals[i] = intervals[(int) labels.get(i) - 97]; // add to sorted intervals according to ASCII value
        }
        char[] buffer = new char[content.length()]; // buffer in which threads will write chunks of text
        for (int i = 0, off = 0; i < numChunks; i++, off += chunkSize) { // create and run a thread for each chunk
            Swapper sw = new Swapper(sortedIntervals[i], content, buffer, off);
            Thread t = new Thread(sw, "Thread" + i);
            t.start(); // start current thread
            try {
                t.join(); // wait for thread
            } catch (Exception e) {
                System.out.println(e.getMessage());
            }
        }
        return buffer;
    }

    /**
     * Writes the buffer to a new file
     * 
     * @param contents string of file content
     * @param chunkSize size of each chunk in the content
     * @param numChunks amount of chunks
     * @throws Exception
     */
    private static void writeToFile(String contents, int chunkSize, int numChunks) throws Exception {
        char[] buff = runSwapper(contents, chunkSize, contents.length() / chunkSize);
        PrintWriter writer = new PrintWriter("output.txt", "UTF-8");
        writer.print(buff);
        writer.close();
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java TextSwap <chunk size> <filename>");
            return;
        }
        String contents = "";
        int chunkSize = Integer.parseInt(args[0]);
        try {
            contents = readFile(args[1]);
            // input checks
            if (contents.length() % chunkSize != 0) {
                System.out.println("Error: file size must be a multiple of the chunk size");
                return;
            }
            if (contents.length() / chunkSize > 26) {
                System.out.println("Error: chunk size too small");
                return;
            }
            // ok to proceed
            writeToFile(contents, chunkSize, contents.length() / chunkSize);
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("Error with IO.");
            return;
        }
    }
}
