package net.wintertideheir.aikaterine

/** A definite, measurable skill.
 *
 * @property description A brief sentence about what the skill is.
 * @property instructions A [String] precisely and thoroughly describing what the skill is and
 *                        how to perform, record, and measure.
 * @property measures An [Array] of [Measure] that the skill will be evaluated on.
 */
data class Skill(val description: String,
                 val instructions: String,
                 val measures: Array<Measure>);