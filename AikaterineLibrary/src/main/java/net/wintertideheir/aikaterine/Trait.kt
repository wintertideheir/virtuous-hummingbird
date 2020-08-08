package net.wintertideheir.aikaterine

import java.io.Serializable
import kotlin.math.PI
import kotlin.math.max

class Trait(var shortDescription: String,
            var longDescription:  String,
            var parent: Trait?,
            var children: MutableList<Trait>) : Serializable {

    @Transient var radius: Float = 0f
    @Transient var angle: Float = 0f

    fun cyclic(visited: MutableList<Trait> = arrayListOf()): Boolean {
        if (this in visited) {
            return true
        }
        visited.add(this)
        return (true in children.map { it.cyclic(visited) })
    }

    fun link(parent: Trait) {
        this.parent = parent
        parent.children.add(this)
    }

    fun delink() {
        parent?.children?.remove(this)
        parent = null
    }

    fun render(angle: Float = 0f,
               sector: Float = 2 * PI.toFloat(),
               radius: Float = 0f) {
        this.angle = angle + (sector / 2)
        this.radius = radius
        children.forEachIndexed { index, child ->
            val childSector = sector / children.size
            val childRadius = max(radius + 1, (PI.toFloat() / 8) / childSector)
            val childAngle = angle + (index * sector / children.size)
            child.render(childAngle, childSector, childRadius)
        }
    }

}