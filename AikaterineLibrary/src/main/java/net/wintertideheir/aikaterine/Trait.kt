package net.wintertideheir.aikaterine

import java.io.Serializable

data class Trait(var shortDescription: String,
                 var longDescription:  String,
                 var parent: Trait?,
                 var children: MutableList<Trait>) : Serializable {

    fun cyclic(visited: MutableList<Trait> = arrayListOf()): Boolean {
        if (this in visited) {
            return true
        }
        visited.add(this)
        for (child in children) {
            if (child.cyclic(visited)) {
                return true
            }
        }
        return false
    }

}