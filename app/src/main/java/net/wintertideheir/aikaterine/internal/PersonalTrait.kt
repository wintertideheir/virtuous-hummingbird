package net.wintertideheir.aikaterine.internal

/** A personal trait, representing either a an abstract quality or a definite skill.
 *  @param shortDescription If a quality, a short name for quick identification.
 *                          If a skill, a brief sentence.
 *                          Ought to be unique in either case.
 *  @param longDescription If a quality, a sentence to discriminate like qualities.
 *                         If a skill, a paragraph of instructions.
 *  @param measure If a quality, set to null.
 *                 If a skill, a function with behaviour yet unspecified.
 *  @param leafTrait A [MutableList] of component [PersonalTrait],
 *                   A skill may never have a quality as a parent.
 */
class PersonalTrait(var shortDescription: String,
                    var longDescription:  String,
                    var measure:          (() -> Unit)? = null,
                    var leafTraits:       MutableList<PersonalTrait> = mutableListOf<PersonalTrait>()) {

    fun isQuality(): Boolean {
        return measure == null;
    }

    fun isChild(shortDescription: String): Boolean {
        return (this.shortDescription == shortDescription) ||
                leafTraits.map {
                    it.isChild(shortDescription)
                }.reduce {
                        a, b -> a || b
                };
    }

}