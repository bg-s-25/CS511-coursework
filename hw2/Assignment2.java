/**
    Title  : CS 511 C - HW Assignment 2
    Desc   : Modelling a gym with clients using thread pool and semaphores
    Name   : Bobby Georgiou
    Date   : 10/06/2019
    Pledge : "I pledge my honor that I have abided by the Stevens Honor System."
 */

/* start the simulation */
public class Assignment2 {
    public static void main(String[] args) {
        Thread thread = new Thread(new Gym());
        thread.start();

        try {
            thread.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
