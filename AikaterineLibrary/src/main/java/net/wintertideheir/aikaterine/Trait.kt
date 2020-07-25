package net.wintertideheir.aikaterine

import java.io.Serializable

class Trait(val id: Int,
            var shortDescription: String,
            var longDescription:  String) : Serializable {

    override fun equals(other: Any?): Boolean {
        return id.equals(other)
    }

    override fun hashCode(): Int {
        return id
    }

}