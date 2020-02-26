# Aikaterine

Aikaterine is an architecture for the organization and advancement of personal quality. It's primary goals are to
1. Organize qualities through categorization and linking in a graph.
2. Associate qualities with scheduled skills in the form of questions and activities.
3. Track progress or regression in qualities by their associated skills.

## Philosophy

Aikaterine's philosophy begins with the premise that people can evaluate their lives numerically and categorically. A person's personal quality is anything of concern that can be measured and improved on. Examples of personal quality are mathematical knowledge, singing ability, physical strength, aesthetic appearance, and visual acuity.

Personal qualities are often abstract and difficult to measure or improve on directly. Skills *are* measurable and improvable. By associating skills with personal qualities, we can track personal qualities. Our observation of skills leads us to conclude that skills progress with repeated use and regress with time. Therefore, personal qualities can progress or regress in the same fashion.

The practical application of this philosophy is that a mental, written, or digital method of tracking and scheduling skills can systematically improve personal qualities.

## Components

Aikaterine is divided into three components that reflect it's primary goals
1. **A quality module that organizes qualities into a graph.** Each node is a quality, and each edge describes how a a certain quality is a part of or subcategory of another. Nodes are measured against associated skills and lower nodes.
2. **A skill module that creates a list of skills and a way to present the skill.** Each skill has an associated node, a unique name, a class, and any necessary class parameters. The class of skill determines how a skill is presented - whether as a flashcard, instructions, or launching another program.
3. **A scheduling module that tracks and schedules skills.** Each class of skill has it's own method of determining how often a skill should be practiced. Skills are then prioritized by their nodes. Skills of the same quality ought to be only highly correlated to one another.
