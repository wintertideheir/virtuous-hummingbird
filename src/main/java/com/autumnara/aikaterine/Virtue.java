package com.autumnara.aikaterine;

import com.autumnara.aikaterine.IllegalCycleException;

import java.util.ArrayList;

/** A virtue. A trait or quality deemed morally good or desirable.
    Anything belonging with a possessive. Virtues are implicitly part
    of a heirarchical tree, the virtue tree, where directed links
    between nodes indicate which virtue is pursued for the sake of the
    other. This relationship can be viewed as relating parts to the
    whole. */
public class Virtue {

    /** The name of the virtue. Ought to be a few words for higher
        virtues. */
    public String            name;
    /** A description of the virtue. Ought to be a sentence for higher
        virtues. */
    public String            description;
    /** The virtue for which we are pursuing this virtue. */
    public Virtue            higher;
    /** The virtues which are pursued for this virtue. */
    public ArrayList<Virtue> lower;

    /** Constructor for a virtue given a name and description. Intended
        for user interfaces where the virtue is constructed before
        being placed. */
    public Virtue(String name,
                  String description)
    {
        this.name        = name;
        this.description = description;
        this.higher      = null;
        this.lower       = new ArrayList<Virtue>();
    }

    /** Constructor for a virtue given a name, description, and higher
        virtue. Intended for the creation of new virtues in user
        interfaces. */
    public Virtue(String name,
                  String description,
                  Virtue higher)
    {
        this.name        = name;
        this.description = description;
        this.higher      = higher;
        this.lower       = new ArrayList<Virtue>();
    }

    /** Complete constructor for a quality. */
    public Virtue(String            name,
                  String            description,
                  Virtue            higher,
                  ArrayList<Virtue> lower)
    {
        this.name        = name;
        this.description = description;
        this.higher      = higher;
        this.lower       = lower;
    }

    /** Constructor for the highest virtue from Aristotlean virtue
        ethics, εὐδαιμονία, often anglicized to eudiamonia. I literally
        copied the name and description from Wikipedia. */
    public static Virtue constructHighest()
    {
        return new Virtue("εὐδαιμονία",
                          "Flourishing, prosperity, or blessedness");
    }

    /** Get the highest virtue in the virtue tree.
        @return The highest virtue in the virtue tree.
        @exception IllegalCycleException Throws an exceptino if
                                         searching for the highest
                                         virtue leads to a cycle. */
    public Virtue getHighest()
    throws IllegalCycleException
    {
        return getHighest(new ArrayList<Virtue>());
    }

    /** Get the highest virtue in the virtue tree given a list of
        visited nodes beneath it. Internal function wrapped by
        the {@link #getHighest() getHighest} method.
        @param visited A list of checked virtues lower than this
                       virtue. The first element of this list ought to
                       be the object that
                       {@link #getHighest() getHighest} was called on,
                       although this is not checked and is
                       nonessential.
        @return The highest virtue in the virtue tree.
        @exception IllegalCycleException Throws an exceptino if
                                         searching for the highest
                                         virtue leads to a cycle. */
    private Virtue getHighest(ArrayList<Virtue> visited)
    throws IllegalCycleException
    {
        if (visited.contains(this))
        {
            throw new IllegalCycleException(visited);
        } else {
            if (this.higher == null)
            {
                return this;
            } else {
                visited.add(this);
                return this.higher.getHighest(visited);
            }
        }
    }

}
