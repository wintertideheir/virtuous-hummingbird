package com.autumnara.aikaterine;

import com.autumnara.aikaterine.ColorRGB;
import com.autumnara.aikaterine.PositionRectangular;

/** <h1> An immutable representation of a virtue. </h1>
    
    <p> A virtue is a trait or quality of a person deemed morally good
    or desirable. Virtues may be assessed by how virtuous acts and
    feelings are, and may be improved through deliberate practice.

    <p> <i>Note:</i> See
    <a href="https://plato.stanford.edu/entries/ethics-virtue/#EudaVirtEthi">
    Eudaimonist virtue ethics</a> for further information.
    */
public class Virtue {

    /** The name of the virtue. */
    protected String name;

    /** A short description of the virtue. */
    protected String description;

    /** The x coordinate of the virtue in 2-dimensional Cartesian
        coordinates. */
    public PositionRectangular pos;

    /** The red component of the color of the virtue when drawn. */
    public ColorRGB color;

    /** Constructor for an immutable virtue. */
    public Virtue(String name,
                  String description)
    {
        this.name        = name;
        this.description = description;
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

}
