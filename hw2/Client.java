/**
    Title  : CS 511 C - HW Assignment 2
    Desc   : Modelling a gym with clients using thread pool and semaphores
    Name   : Bobby Georgiou
    Date   : 10/06/2019
    Pledge : "I pledge my honor that I have abided by the Stevens Honor System."
 */

import java.util.List;
import java.util.ArrayList;
import java.util.Random;

public class Client {
    private int id;
    private List<Exercise> routine;

    public Client(int id) {
        this.id = id;
        this.routine = new ArrayList<Exercise>();
    }

    /**
     * Adds an exercise object to the routine list
     * @param e the exercise to add
     */
    public void addExercise(Exercise e) {
        routine.add(e);
    }

    /**
     * Getter for routine
     * @return Client.routine
     */
    public List<Exercise> getRoutine() {
        return routine;
    }

    /**
     * Getter for id
     * @return Client.id
     */
    public int getId() {
        return id;
    }

    /**
     * Generate a Client with a random exercise routine (15-20 exercises) and assign it the given id
     * @param id the id of the Client
     * @return the new Client
     */
    public static Client generateRandom(int id) {
        Client c = new Client(id);
        int numExercises = 15 + new Random().nextInt(6); // 15-20 exercises per client
        for (int i = 0; i < numExercises; i++) {
            c.addExercise(Exercise.generateRandom());
        }
        return c;
    }
}
