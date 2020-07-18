package net.wintertideheir.aikaterine.internal

/** A personal trait, either a quality of skill.
 *
 * @property shortDescription A short, nonempty [String] to describe the personal trait.
 *                            Ought to become longer with increasing depth.
 * @property longDescription An optional description longer than [shortDescription].
 * @property parent A non-negative [Int] index of the parent node in the [PersonalTraitTree].
 */
class PersonalTrait(var shortDescription: String,
                    var longDescription:  String,
                    var parent:           Int)