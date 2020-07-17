package net.wintertideheir.aikaterine.internal

/** A personal trait, representing either a an abstract quality or a definite skill.
 *  @param shortDescription If a quality, a short name for quick identification.
 *                          If a skill, a brief sentence.
 *                          Ought to be unique in either case.
 *  @param longDescription If a quality, a sentence to discriminate like qualities.
 *                         If a skill, a paragraph of instructions.
 *  @param measure If a quality, set to null.
 *                 If a skill, a function with behaviour yet unspecified.
 */
class PersonalTrait(var shortDescription: String,
                    var longDescription:  String,
                    var measure:          (() -> Unit)?) {
}