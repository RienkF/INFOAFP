# INFOAFP - Project

Gijs Blanken, Rienk Fidder, Martin Tjon

## Running

To run the backend, navigate to the submit-hs folder, and run `cabal run`

## Running the frontend

The frontend is precompiled, and can accessed by performing a GET request to the root of the application. Assuming elm is installed, the frontend can also be run seperately by navigating to the submit-hs-frontend folder and running `elm reactor` for a development server, or `elm make src/Main.elm` to compile the frontend, and then accessing the resulting html file.
