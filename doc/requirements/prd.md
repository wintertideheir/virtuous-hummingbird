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

### Business Purpose

### Stakeholders

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
