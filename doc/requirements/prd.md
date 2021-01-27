# Products Requirements Document for Virtuous Hummingbird

## Table of Contents
1. Introduction
2. Business Requirements
    1. Business Purpose
    2. Stakeholders
    3. Business Requirements
3. Market Assessment
    1. Target Demographic
    2. Target Problem
    3. Competing and Comparable Products
4. Product Details
    1. Product Overview
    2. Functional Requirements
    3. Usability Requirements
    4. Technical Requirements
    5. Environmental Requirements
5. Supporting Data
    1. Assumptions
    2. Constraints
    3. Dependencies
6. Conclusion

## Introduction

Virtuous Hummingbird is a program for the organization and advancement of personal virtue.
By virtue, we mean the virtues of virtue ethics, personal qualities indicating a pattern of repeated behavior.
These qualities are related causally to form a hierarchical tree of virtues. 
The simple nature of virtues and virtue trees makes virtue ethics an unusually intuitive and practical ethical theory.
By presenting a way to create, edit, visualize, and track these virtues, Virtuous Hummingbird facilitates living a virtuous life.

## Business Requirements

The first person (I/me/myself) refers to [wintertideheir](www.github.com/wintertideheir).

### Business Purpose

There are several reasons to create Virtuous Hummingbird, in order of descending priority:

1. __To create a prototype for a more advanced version of Virtuous Hummingbird.__
   As far as I know, there is no analogous software or system in the public view.
   Without a basic example to refine, I can't be sure that this idea has any merit.
   I can't even base our priors on existing productivity software, because Virtuous Hummingbird is _ethically_ focused.
   Ethics encompasses more of life than productivity, and is therefore exponentially more complex.
   An ethical process also has a greater inherent chance for failure, unrelated to it's complexity, as it's a scientific process, rather than an engineering process.
   Productivity can be engineered in response to a set of requirements, but ethics must be refined through science.
   Given all these factors for failure, a prototype can show both where the ethical theory and the software can be improved.

2. __As a portfolio example.__
   Virtuous Hummingbird can serve as a programming portfolio piece, particularly as an example of the waterfall method.
   I don't have any other complete, well designed projects to showcase my programming ability, and I might want to apply for a software engineering related position in the future.
   By selecting a modern language and modern tooling (presently Java 8 and Gradle), I can show that I am up-to-date.
   If I choose to rewrite Virtuous Hummingbird in another language, I can demonstrate agility and knowledge with multiple programming languages and environments.
   These different approaches to demonstrating competence should be easy if I perform the waterfall correctly.

3. __To live a more virtuous life.__
   Virtuous Hummingbird is likely too simple to be a truly effective tool of virtue.
   The underlying ethical theory is well-developed and explored, given that eudaimonist virtue theory was developed in ancient Greece.
   Selecting a specific moral theory, however, is a matter of science, not of art or mathematics.
   The mathematical approach to morality posits that ethics can be argued and that a single ethical theory can become universally accepted based on logic alone.
   This approach has clearly been disproved on a practical level by the length of philosophy.
   Neither can ethics be considered an art, because ethical theories require internal logical consistency.
   Ethics appears to me to be a process of testing out which ethical theories match our personal conception of a good life.
   As we become better aligned with our suited ethical theory, we reinforce our belief in our correctness.
   With this view, it's clear that there is no other path to making life virtuous and to creating a tool to do so than through a cycle of constant iteration and improvement in both.

### Stakeholders

The only stakeholder presently associated with Virtuous Hummingbird is me.
However, by dedicating time and effort to this project, I am indirectly affecting my present and future.
As I am working on a few other projects, software-related and -unrelated, I must ensure that I am allocating time efficiently above all.
Future endeavors also depend on quick turnover of previous projects.

### Business Requirements

## Market Assessment

### Target Demographic

The target demographic is between 25 and 35 years of age.
This age group has the maturity to recognize the value of an ethical system, and enough time to reap the benefits of a full implementation.

The target demographic is male, because young men appear to take greater risks and have less of a sense of purpose in life.
The downsides of generally greater risk taking can be best mitigated by carefully selecting which risks have the highest return on investment, a task any ethical theory can help with.
Virtue ethics in particular favors long-term develop of the virtues, and therefore uniquely discourages short-term risk-taking.

The target demographic should also be educated, with at least some college, but preferably a bachelor's degree.
Virtue ethics works best when applied to a varied and numerous set of decisions, because their sheer potential complexity requires a deliberately simple approach.
College-educated users manage intellectually challenging classes and/or jobs, which therefore requires managing a varied and numerous set of decisions.

The target demographic is unmarried and without dependents, and preferably single.
As a consequence they will spend less time on relationships that will likely not benefit from ethical theory.

The target demographic values an individualistic approach to life and a Protestant work ethic.
Individualism is the principle that one ought to be independent and self-reliant, which encourages self-directed learning.
The Protestant work ethic is the belief that work, discipline, and frugality are a divine obligation.
A Protestant work ethic requires the hard work needed to make use of virtue ethics.

### Target Problem

Our target demographic will likely have problems efficiently managing their time.
In many cases, a combination of a calendar, task-list, and time-tracker can help plan, prioritize, and track their activities.
As the the number and variety of activities increases, however, optimally prioritizing them becomes more difficult.
Virtue ethics can help this problem by offering a long-term, continuous, and simple method of organization.

First, these activities must be integrated into a tree of virtues.
A virtue is a trait with an associated pattern of behavior.
Because nearly all virtues are done for the sake of other virtues, we can represent virtues as a two-dimensional node graph.
The user constructs these virtue trees, assigns initial weights, and then begins recording the consequences of activities.

Virtuous Hummingbird will then learn which virtues the user values the most and present a set of activities that best reflect the virtue tree.
In essence, Virtuous Hummingbird is a form of machine learning applied to task prioritization with an element of deliberation.
By combining hard work and automation, task prioritization is easy.

### Competing and Comparable Products

The products that offer the closest functionality to Virtuous Hummingbird are task trackers.
Task trackers expand on the basic task-list with features such as categories, tags, and prioritization.
Their primary advantage is their simplicity of use.
Lists are very simple to create, edit, and view.
Their primary disadvantage is their difficulty in scaling as the number and variety of tasks grows.
Task-trackers attempt to mitigate their scaling problems with organization tools, requiring at least a little planning.
There are several popular task-trackers available as of January 2021:

* __Google Tasks__, _free_.
  A widely used product on account of it being bundled with the standard apps for a Google account.
  It provides the ability to create sub-tasks, set due dates, and send notifications.
  It integrates with other Google apps, namely Gmail and Google Calendar.
* __Microsoft To Do__, _from $5 per user per month_.
  This product appears to offer similar features to Google Tasks.
* __Remember The Milk__, _free_ or _$40 per year_.
  A simple task-list service with a focus on everyday, non-business application.
  Features an advanced auto-complete feature that makes creating tasks convenient.
  It can use e-mail, text, Twitter, and other apps.
  It can connect files from Google Drive and Dropbox to tasks.
  It features a smart search feature and priority system.
* __Trello__, _free_ to _$17.5 per user per month_.
  A professional-focused task tracker.
  Based around dividing tasks into teams.
  These tasks can be detailed with comments, attachments, and due dates, and then viewed with an sophisticated interface.
  Programming and automation is available to streamline that process as well.
* __Todoist__, _free_ to _$5 per user per month_.
  A task-tracker with many interesting features.
  It's core feature are well-detailed tasks.
  It also allows collaboration on tasks, including delegation and comments.
  Gamification is present as well, with a karma system and productivity visualizations.
* __Asana__, _free_ to _per contract pricing_.
  A feature-risk task-tracker with many different ways of viewing a task list, including a time-line view, a table view, a multi-list view, etc.
  It also features programming / automation to streamline task creation and management.

Another class of products to consider are time-tracking apps.
Rather than planning an activity in advance, time-tracking apps record an activity as it occurs.
They often have similar features to task-trackers, as well as billing, export, and retrospective editing features.
There are several popular time-tracking apps, as of January 2021:

* __Toggl Track__, _free_ or _$9 per user per month_.
  An easy-to-use tracking app.
  Activities can be tracked with or without a name, project, or tags, and then edited later.
  It also integrates very well with Google apps and works on many different platforms.
* __Harvest__, _free_ or _$12 per user per month_.
  A team focused time-tracker.
  Teams can connect their logs to an administration log.
  It can also integrate well with task-trackers and other apps.
* __Everhour__, _free_ or _$5 per user per month_.
  A light-weight time-tracker.
  It integrates well with other productivity apps and offers employee integration.

Calendar applications often provide very simple versions of the functionality mentioned above.
Full capability is provided only in conjunction with the above apps.

## Product Details

### Product Overview

### Functional Requirements

__Requirement 1__
The application must only use one window.
The primary reason for this requirement is to reduce dependence on a tiling window manager.
This requirement also reduces the complexity of multiple windows and multiple UI components.

__Requirement 2__
The application must be divided into three easily accessible views,
* the virtue tree view,
* the recording view,
* and the statistics and recommendation view.

This division should mirror the three different functions of Virtuous Hummingbird.
The virtue tree view should be dedicated to creating and organizing virtues.
The recording view should be dedicated to recording activities or evaluations of virtues.
The statistics and recommendation view should be dedicated to general statistical trends on the virtue tree and recommendations on which virtues to practice next.

__Requirement 2.1__
The virtue tree view must be composed of three parts,
* an editing and viewing component to view and edit the attributes of virtues,
* a virtue tree graph view to display parts of a virtue tree,
* a settings component to save and load files, change settings, etc.

__Requirements 2.2__
The recording view must have the following features:
* A large, conspicuous multipurpose button that cycles between the following states:
  * A pre-recording state, shown as a play button.
    Activating the button should begin recording a new activity session.
    This state should be active by default unless overridden by other states.
  * A recording state, shown as a stop button.
    Activating the button should end the current recording.
    The button should only be active when there is an active recording that was started by the pre-recording state.
  * A submission state, shown as a check button.
    Activating the button should submit a new activity session with the given parameters.
    This state should be only be active when the user submits enough parameters to create a valid activity session. 

### Usability Requirements
### Technical Requirements
### Environmental Requirements
## Supporting Data
### Assumptions
### Constraints
### Dependencies
## Conclusion
