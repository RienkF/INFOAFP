# INFOAFP - Project proposal

Rienk Fidder - Gijs Blanken - Martin TODO

## Domain

The domain we want to work on is that of project submissions via a submissions system. Our aim is to create a webserver with a frontend that allows a teacher to create assignments, and students to submit their solutions for these assignments. The system will be focussed on submissions of programming assignments, much like an application such as DOMjudge. We want to start by allowing the application to only accept haskell submissions, and compiling these such that predefined in and outputs can be tested. we want teachers to be able to specify constraints for the in and outputs such that Quickcheck can verify if these properties hold for a given program.

To keep the project manageble within the given time, we will leave authentication out of the scope, and will focus on a single classroom. If time allows it we want to be able to time the submissions and show a scoreboard of the fastest programs.

## Problems we want to solve

The problem our library will try to solve follows clearly from the domain, to allow teachers to manage assignments and give students a centralized environment to hand in assignments and receive feedback on them. This process will be automated by the use of test cases and properties (checked by Quickcheck) that the teacher can define.

This will also solve the problem of keeping track of grades, as the system will allow the teacher to view for each assignment a list of grades linked to the students.

## Estimated schedule

### 01/03

A project setup that has a working installation of a webserver library and can be run to expose a simple "hello world" page

### 08/03

A connection to a database so that assignments can be stored, and tables in this database to store an assignment, the solutions handed in by students so far, and the scores for these submissions.

### 15/03

Webpages for the different components of the system, allowing students to hand in a hs file and allow it to be stored in the database, and allowing teachers to create assignments with test cases.

### 22/03

A way to compile the hs files and inputting a test case.

### 27/03

A timing of the solution and a score board displaying the current best times.

### 05/04

The finalized project
