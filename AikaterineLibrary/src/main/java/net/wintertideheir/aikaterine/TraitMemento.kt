package net.wintertideheir.aikaterine

import java.io.Serializable

interface TraitMemento : Serializable {

    fun apply(traitTree: net.wintertideheir.aikaterine.TraitTree)

}