package net.wintertideheir.aikaterine

import java.io.Serializable

class Trait(var shortDescription: String,
            var longDescription:  String,
            var parent: Trait?,
            var children: MutableList<Trait>) : Serializable {

    fun cyclic(visited: MutableList<Trait> = arrayListOf()): Boolean {
        if (this in visited) {
            return true
        }
        visited.add(this)
        return (true in children.map { it.cyclic(visited) })
    }

}