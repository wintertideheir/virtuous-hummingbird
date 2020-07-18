package net.wintertideheir.aikaterine.internal

/** A change to a personal trait represented as a partial [PersonalTrait]. Each nullable property
 *  corresponds to a property in [PersonalTrait]. A null value for these properties indicates no
 *  change. If a corresponding [PersonalTrait] does not exist with the given index, it will be
 *  created.
 *
 * @property shortDescription A change to the short description.
 *                            If equal to an empty string, indicates deletion of the trait.
 * @property longDescription A change to the long description.
 * @property parent A change of parent node.
 * @property index The index of the [PersonalTrait] in the [PersonalTraitTree]. Cannot be reserved
 *                 indices 0 and 1.
 */
class PersonalTraitChange (val shortDescription: String? = null,
                           val longDescription:  String? = null,
                           val parent:           Int?    = null,
                           val index:            Int) {
    init {
        if (index == 0) {
            throw Exception("Forbidden change to workspace root node.");
        }
        if (index == 1) {
            throw Exception("Forbidden change to \"Eudaemonia\" root node.");
        }
    }
}