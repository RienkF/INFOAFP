---
title: 'Online automated submission system'
subtitle: 'INFOAFP: group project first proposal'
author:
 - 'Rienk Fidder'
 - 'Gijs Blanken'
 - 'Martin Tjon'
---

## Domain

The domain we want to work on is that of programming assignment grading via a
submissions system. Our aim is to create a webinterface where teachers and
students can create and hand in assignments, respectively, much like an
application like DOMjudge. We will, however, be leveraging QuickCheck for
property-based testing and create a system such that the teacher can specify
these properties from the webinterface.

In terms of scoping, we need to ensure that the project is manageble within the
constraints of the course. Therefore, we will focus on a single classroom and
forego considering Identity and Access Management, multiple classrooms, etc. If
time allows it we would like to be able to time the submissions and show a scoreboard
of the fastest programs. 


## Problems we want to solve

The problem our application will try to solve follows clearly from the domain; 
allowing teachers to manage programming assignments and give students a
centralized environment to hand in said assignments and receive feedback on
them. This process will be automated by the use of test cases and properties
(checked by QuickCheck) that the teacher can define.

This will also solve the problem of keeping track of grades, as the system will
allow the teacher to view for each assignment a list of grades linked to the
students. 


## Estimated schedule

NOTE: this is a tentative schedule and can be altered depending on feedback or
further insight in the complexity of specific components.


### 01/03

Initial high-level design of the application, including components&mdash;e.g.
webserver, database, objectstore, etc.&mdash;and a rough idea of the UX we wish
to achieve.

Project organization (tickets, issues, milestones).

A project scaffold including a working installation of a webserver library and
can be run to expose a simple "hello world" page


### 08/03

A connection to a database so that assignments can be stored, and tables in
this database to store an assignment, the solutions handed in by students so
far, and the scores for these submissions.


### 15/03

Webpages for the different components of the system, allowing students to hand
in a `.hs` file and allow it to be stored, and allowing teachers to create
assignments with test cases. 


### 22/03

A way to compile the hs files and inputting a test case. TODO: we may want to
move this part up in the timeline, as it is crucial and I feel it is the more
complex part.


### 27/03

Time allowing: A timing of the solution and a score board displaying the
current best times.


### 05/04

The finalized project
