package net.wintertideheir.aikaterine

import java.io.Serializable

interface TraitChange : Serializable {

    fun apply(traitTree: net.wintertideheir.aikaterine.TraitTree)

}