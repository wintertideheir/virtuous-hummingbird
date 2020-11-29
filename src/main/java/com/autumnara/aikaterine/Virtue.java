package com.autumnara.aikaterine;

import java.io.Serializable;

/** <h1> An immutable representation of a virtue. </h1>
    
    <p> A virtue is a trait or quality of a person deemed morally good
    or desirable. Virtues may be assessed by how virtuous acts and
    feelings are, and may be improved through deliberate practice.

    <p> <i>Note:</i> See
    <a href="https://plato.stanford.edu/entries/ethics-virtue/#EudaVirtEthi">
    Eudaimonist virtue ethics</a> for further information.
    */
public class Virtue implements Serializable {

    /** The name of the virtue. */
    protected String name;

    /** A short description of the virtue. */
    protected String description;

    /** Whether the virtue is primary or secondary. Secondary virtues
        are only shown while editing. */
    protected boolean primary;

    /** The x coordinate of the virtue in 2-dimensional Cartesian
        coordinates. */
    public transient float x = 0;

    /** The y coordinate of the virtue in 2-dimensional Cartesian
        coordinates. */
    public transient float y = 0;

    /** Constructor for an immutable virtue. */
    public Virtue(String  name,
                  String  description,
                  boolean primary)
    {
        this.name        = name;
        this.description = description;
        this.primary     = primary;
    }

    /** Get the {@link Virtue#name} of the virtue. */
    public String getName()
    {
        return this.name;
    }

    /** Get the {@link Virtue#description} of the virtue. */
    public String getDescription()
    {
        return this.description;
    }

    /** Get whether the virtue is {@link Virtue#primary}. */
    public boolean getPrimary()
    {
        return this.primary;
    }

}
