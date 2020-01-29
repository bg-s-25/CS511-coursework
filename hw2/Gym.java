/**
    Title  : CS 511 C - HW Assignment 2
    Desc   : Modelling a gym with clients using thread pool and semaphores
    Name   : Bobby Georgiou
    Date   : 10/06/2019
    Pledge : "I pledge my honor that I have abided by the Stevens Honor System."
 */

import java.util.Arrays;
import java.util.Random;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Semaphore;

public class Gym implements Runnable {
    private static final int GYM_SIZE = 30;
    private static final int GYM_REGISTERED_CLIENTS = 10000;
    private Map<WeightPlateSize, Integer> noOfWeightPlates;
    private Set<Integer> clients;
    private ExecutorService executor;

    // semaphore declarations - per apparatus type and weight plate size
    private final Semaphore[] semApparatus = new Semaphore[8];
    private final Semaphore[] semWeightPlates = new Semaphore[3];

    /**
     * Run all client routines, utilizing semaphores, as a fixed thread pool
     */
    public void run() {
        // setup
        Arrays.fill(semApparatus, new Semaphore(1));
        clients = new HashSet<Integer>();
        noOfWeightPlates = new HashMap<WeightPlateSize, Integer>();
        noOfWeightPlates.put(WeightPlateSize.SMALL_3KG, 110);
        noOfWeightPlates.put(WeightPlateSize.MEDIUM_5KG, 90);
        noOfWeightPlates.put(WeightPlateSize.LARGE_10KG, 75);
        semWeightPlates[0] = new Semaphore(noOfWeightPlates.get(WeightPlateSize.SMALL_3KG)); // permits based on noOfWeightPlates
        semWeightPlates[1] = new Semaphore(noOfWeightPlates.get(WeightPlateSize.MEDIUM_5KG));
        semWeightPlates[2] = new Semaphore(noOfWeightPlates.get(WeightPlateSize.LARGE_10KG));

        // generate clients and execute thread pool
        executor = Executors.newFixedThreadPool(GYM_SIZE);
        Client[] clientsArr = new Client[GYM_SIZE];
        int curIndex = 0;
        while (clients.size() < GYM_SIZE) {
            int newid; // random id based on number of registered clients
            if (clients.add(newid = new Random().nextInt(GYM_REGISTERED_CLIENTS))) {
                clientsArr[curIndex] = Client.generateRandom(newid);
                curIndex++;
            }
        }

        executor.execute(new Runnable() {
            public void run() {
                // run routine of each client; do acquires and releases
                for (Client c : clientsArr) {
                    int curApparatusIndex, curPlateCnt;
                    for (Exercise e : c.getRoutine()) {
                        // acquire appropriate permission(s)
                        curApparatusIndex = e.getAt().ordinal();
                        try {
                            System.out.println("Client" + c.getId() + " acquires " + e.getAt().toString());
                            semApparatus[curApparatusIndex].acquire();
                        } catch (InterruptedException ie) {
                            ie.printStackTrace();
                        }
                        for (int plate = 0; plate < 3; plate++) {
                            curPlateCnt = e.getWeight().get(WeightPlateSize.values()[plate]);
                            for (int i = 0; i < curPlateCnt; i++) { // acquire the specified number of plates
                                try {
                                    System.out.println("Client" + c.getId() + " acquires " + WeightPlateSize.values()[plate].toString());
                                    semWeightPlates[plate].acquire();
                                } catch (InterruptedException ie) {
                                    ie.printStackTrace();
                                }
                            }
                        }

                        // do exercise for provided duration (thread sleeps)
                        try {
                            System.out.println("Client" + c.getId() + " exercises for " + e.getDuration() + " ms");
                            Thread.sleep(e.getDuration());
                        } catch (InterruptedException ie) {
                            ie.printStackTrace();
                        }
                        
                        // release appropriate permission(s)
                        for (int plate = 0; plate < 3; plate++) {
                            curPlateCnt = e.getWeight().get(WeightPlateSize.values()[plate]);
                            for (int i = 0; i < curPlateCnt; i++) {
                                System.out.println("Client" + c.getId() + " releases " + WeightPlateSize.values()[plate].toString());
                                semWeightPlates[plate].release();
                            }
                        }
                        System.out.println("Client" + c.getId() + " releases " + e.getAt().toString());
                        semApparatus[curApparatusIndex].release();
                    }

                    clients.remove(c.getId());
                }
            }
        });
        executor.shutdown(); // do not accept new tasks, shut down ExecutorService
    }
}
