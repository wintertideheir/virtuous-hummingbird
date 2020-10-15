package com.autumnara.aikaterine;

import java.util.ArrayList;

/** Describes a duplicate element in what ought to be a set. */
public class DuplicateElementException extends Exception {

    public <S> DuplicateElementException(S duplicate)
    {
        super("Duplicate element " + duplicate.toString() + ".");
    }

}
