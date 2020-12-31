package com.autumnara.aikaterine;

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
public class Virtue
{

    /** The unique name of the virtue.
      */
    public String name;

    /** A short description of the virtue.
      */
    public String description;

    /** Constructor for a virtue.
      */
    public Virtue(String name,
                  String description)
    {
        this.name        = name;
        this.description = description;
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
