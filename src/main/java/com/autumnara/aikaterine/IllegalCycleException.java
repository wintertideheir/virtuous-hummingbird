package com.autumnara.aikaterine;

import java.util.ArrayList;

/** Describes a cycle in what ought to an acyclic graph. */
public class IllegalCycleException extends Exception {

    /** Constructor where the exact path of the cycle is known.
        @param cycle An ordered list of the cycle, beginning and ending
                     with the same element. */
    public <S> IllegalCycleException(ArrayList<S> cycle)
    {
        super("Illegal cycle in acyclic graph, " +
              String.join(" -> ", cycle.stream()
                                       .map(o -> o.toString())
                                       .toArray(String[]::new)));
    }

}
