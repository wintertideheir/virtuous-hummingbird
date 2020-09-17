package com.autumnara.aikaterine;

import java.util.ArrayList;

/** A quality of an individual. Anything belonging with a possessive.
    Qualties are implicitly part of a heirarchical tree, the quality
    tree, where directed links between nodes indicate which quality is
    pursued for the sake of the other. This relationship can be viewed
    as relating parts to the whole. */
public class Quality {

    /** The name of the quality. Ought to be a few words for higher
        qualities. */
    public String             name;
    /** A description of the quality. Ought to be a sentence for higher
        qualities. */
    public String             description;
    /** The quality for which we are pursuing this quality. */
    public Quality            higher;
    /** The qualities which are pursued for this quality. */
    public ArrayList<Quality> lower;

    /** Constructor for a quality given a name and description.
        Intended for user interfaces where the quality is constructed
        before being placed. */
    public Quality(String name,
                   String description)
    {
        this.name        = name;
        this.description = description;
        this.higher      = null;
        this.lower       = new ArrayList<Quality>();
    }

    /** Constructor for a quality given a name, description, and higher
        quality. Intended for the creation of new qualities in user
        interfaces. */
    public Quality(String name,
                   String description,
                   Quality higher)
    {
        this.name        = name;
        this.description = description;
        this.higher      = higher;
        this.lower       = new ArrayList<Quality>();
    }

    /** Complete constructor for a quality. */
    public Quality(String             name,
                   String             description,
                   Quality            higher,
                   ArrayList<Quality> lower)
    {
        this.name        = name;
        this.description = description;
        this.higher      = higher;
        this.lower       = lower;
    }

    /** Constructor for the highest virtue, the root of the quality
        tree, from Aristotlean virtue ethics, εὐδαιμονία, often
        anglicized to eudiamonia. I literally copied the name
        description from Wikipedia. */
    public static Quality constructHighest()
    {
        return new Quality("εὐδαιμονία",
                           "Flourishing, prosperity, or blessedness");
    }

}
