/**
    Title  : CS 511 C - HW Assignment 1
    Desc   : Concurrent file writing
    Name   : Bobby Georgiou
    Date   : 05/15/2019
    Pledge : "I pledge my honor that I have abided by the Stevens Honor System."
 */

public class Swapper implements Runnable {
    private int offset;
    private Interval interval;
    private String content;
    private char[] buffer;

    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }

    /**
     * Thread action - writes specified content into the buffer
     */
    @Override
    public void run() {
        // place stuff in buffer where it belongs
        int bufferPos = offset;
        for (int pos = interval.getX(); pos <= interval.getY(); pos++) {
            buffer[bufferPos] = content.charAt(pos);
            bufferPos++;
        }
    }
}
