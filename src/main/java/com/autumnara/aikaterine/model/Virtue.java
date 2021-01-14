package com.autumnara.hummingbird.model;

/** A virtue.
  * 
  * A virtue is a trait or quality of a person deemed morally good
  * or desirable. Virtues may be assessed by how virtuous acts and
  * feelings are, and may be improved through deliberate practice.
  * 
  * <p> <i>Note:</i> See
  * <a href="https://plato.stanford.edu/entries/ethics-virtue/#EudaVirtEthi">
  * Eudaimonist virtue ethics</a> for further information.
  *
  */
class Virtue
{

    /** The unique name of the virtue.
      */
    String name;

    /** A short description of the virtue.
      */
    String description;

    /** The position of this virtue when drawn in two-dimensional
      * space.
      */
    Position position;

    /** The color associated with this virtue.
      */
    Color color;

    /** Constructor for a virtue.
      */
    Virtue(String   name,
           String   description,
           Position position,
           Color    color)
    {
        this.name        = name;
        this.description = description;
        this.position    = position;
        this.color       = color;
    }

    /** {@inheritDoc}
      *
      * Only compares the {@link #name names} of the operands, if both
      * are virtues.
      */
    @Override
    public boolean equals(Object obj)
    {
        if (obj == null)
        {
            return false;
        }

        if (obj.getClass() != this.getClass()) {
            return false;
        }

        return this.name == (((Virtue)obj).name);
    }

    @Override
    public int hashCode()
    {
        return this.name.hashCode();
    }

}
