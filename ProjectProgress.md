---
title: 'Online automated submission system'
subtitle: 'INFOAFP: project update'
author:
 - 'Rienk Fidder'
 - 'Gijs Blanken'
 - 'Martin Tjon'
---

# Data types and techniques

Our current implementation mainly relies on a set of simple data records storing information such as users, classrooms, assignments, submissions, attempts, and gradings. These data types implement the generic class, so that we can use [Beam](https://haskell-beam.github.io/beam/) as an ORM to create and communicate with an SQLite database. This creationg of the SQLite schema is done via [beam-migrate](https://hackage.haskell.org/package/beam-migrate). We then use [Servant](https://docs.servant.dev/en/stable/index.html) to create a web server that exposes (for now) rather simple CRUD operations on these datatypes as a REST Api, and exposes our frontend.

We use [Elm](https://elm-lang.org/) to create a frontend user interface to allow users to more easily communicate with this api.

# Progress

We have decided on our primary techstack, implemented the [database schema and
migrations](https://github.com/RienkF/INFOAFP/pull/9) through Beam and setup
basic scaffolding for the server and
[front-end](https://github.com/RienkF/INFOAFP/pull/10). We ran into issues
getting Beam, and primarily the migration running. 

We also intended to use OpenAPI Spec to generate the server routing and
front-end calls. While we had it working using the OpenAPI spec generators for
Elm (client) and Haskell (server), this is incompatible with the work we have
done so far in terms of our database. We will abandon this approach and focus
on hand-writing the routing and API calls.

Our repository can be found [here](https://github.com/RienkF/INFOAFP/)

# Timeline update
We are slightly behind schedule due to some unforseen issues (particulary with 3rd party libaries). Some of the lost time was invested in getting our migration tool working which should result in faster iteration cycles in the future. We might have to scrap the timing of a solution which was proposed to be done in the last week of the project. This would allow us to shift the schedule one week forward putting us right on track. 
