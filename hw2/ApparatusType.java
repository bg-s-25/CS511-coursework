/**
    Title  : CS 511 C - HW Assignment 2
    Desc   : Modelling a gym with clients using thread pool and semaphores
    Name   : Bobby Georgiou
    Date   : 10/06/2019
    Pledge : "I pledge my honor that I have abided by the Stevens Honor System."
 */

import java.util.Random;

public enum ApparatusType {
    LEGPRESSMACHINE, BARBELL, HACKSQUATMACHINE, LEGEXTENSIONMACHINE, 
    LEGCURLMACHINE, LATPULLDOWNMACHINE, PECDECKMACHINE, 
    CABLECROSSOVERMACHINE;

    /**
     * Generate a random ApparatusType from the enum
     * @return the random ApparatusType
     */
    public static ApparatusType randomApparatusType() {
        return ApparatusType.values()[new Random().nextInt(ApparatusType.values().length)];
    }
}
