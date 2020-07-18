package net.wintertideheir.aikaterine.internal

class PersonalTraitTree(val tree: MutableMap<Int, PersonalTrait> = hashMapOf()) {

    init {
        tree[0] = PersonalTrait("Workspace", "Abstract root for all nodes.", 0);
        tree[1] = PersonalTrait("Eudaemonia", "Psuedo-root for all non-orphaned nodes.", 0);
    }

    fun apply(change: PersonalTraitChange) {
    }

}