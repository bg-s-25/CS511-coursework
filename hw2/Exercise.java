/**
    Title  : CS 511 C - HW Assignment 2
    Desc   : Modelling a gym with clients using thread pool and semaphores
    Name   : Bobby Georgiou
    Date   : 10/06/2019
    Pledge : "I pledge my honor that I have abided by the Stevens Honor System."
 */

import java.util.Map;
import java.util.HashMap;
import java.util.Random;

public class Exercise {
    private ApparatusType at;
    private Map<WeightPlateSize, Integer> weight;
    private int duration;

    public Exercise(ApparatusType at, Map<WeightPlateSize, Integer> weight, int duration) {
        this.at = at;
        this.weight = weight;
        this.duration = duration;
    }

    /**
     * Getter for at
     * @return Exercise.at
     */
    public ApparatusType getAt() {
        return at;
    }

    /**
     * Getter for weight
     * @return Exercise.weight
     */
    public Map<WeightPlateSize, Integer> getWeight() {
        return weight;
    }

    /**
     * Getter for duration
     * @return Exercise.duration
     */
    public int getDuration() {
        return duration;
    }

    /**
     * Generate a random Exercise consisting of an apparatus type, weight plate sizes mapped to integers, and a duration (1-10 min)
     */
    public static Exercise generateRandom() {
        Map<WeightPlateSize, Integer> rndWeightMap = new HashMap<WeightPlateSize, Integer>();
        Random rndWeight = new Random();
        Integer[] weightDurations = new Integer[3];
        
        int weightTotal = 0;
        while (weightTotal == 0) { // if weightTotal remains 0, try again
            for (int i = 0; i < 3; i++) {
                weightDurations[i] = rndWeight.nextInt(10);
                weightTotal += weightDurations[i];
            }
        }

        rndWeightMap.put(WeightPlateSize.SMALL_3KG, weightDurations[0]);
        rndWeightMap.put(WeightPlateSize.MEDIUM_5KG, weightDurations[1]);
        rndWeightMap.put(WeightPlateSize.LARGE_10KG, weightDurations[2]);
        
        return new Exercise(ApparatusType.randomApparatusType(), rndWeightMap, 1 + new Random().nextInt(10));
    }
}
