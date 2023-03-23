---
title: 'Online automated submission system'
subtitle: 'INFOAFP: project update'
author:
 - 'Rienk Fidder'
 - 'Gijs Blanken'
 - 'Martin Tjon'
---

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

