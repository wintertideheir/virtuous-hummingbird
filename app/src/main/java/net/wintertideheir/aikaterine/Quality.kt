package net.wintertideheir.aikaterine

/** An abstract quality, virtue, or talent of an individual.
 *
 * @property name A short, unique [String] for quick identification of the quality.
 * @property description A sentence describing the quality in enough detail
 *                       to merge or branch qualities.
 * @property skills A list of associated skills.
 */
data class Quality(var name: String,
                   var description: String,
                   var skills: Array<Skill>);
