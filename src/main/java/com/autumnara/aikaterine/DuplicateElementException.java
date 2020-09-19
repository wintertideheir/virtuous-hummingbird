package com.autumnara.aikaterine;

import java.util.ArrayList;

/** Describes a duplicate element in what ought to be a set. */
public class DuplicateElementException extends Exception {

    /** Constructor for a duplicate element.
        @param duplicate The duplicate element. */
    public <S> DuplicateElementException(S duplicate)
    {
        super("Duplicate element " + duplicate.toString() + ".");
    }

}
