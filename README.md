# DMOTE wrist rest topography generator

This is a CLI script which will output a three-dimensional topography in the
simple format expected by the `surface()` function in
[OpenSCAD](http://www.openscad.org/).

It is intended to be used with the DMOTE keyboard project, specifically in
generating the surface of a wrist rest to be cast in a soft material.

Because a wrist rest should have fairly smooth organic curves, the topography
is based on a [bivariate normal distribution
function](https://en.wikipedia.org/wiki/Multivariate_normal_distribution)
multiplied by a logarithmic mound shape.

## Usage

You need Leiningen and Clojure to run this program from your terminal.
For help with the parameters, try this, where $ represents your prompt:

    $ lein run -- -h

The output goes to your terminal. You will want to redirect it to a
file. Read the `Makefile` for a Linux-style example.

## Known errors

A lot of sizes produce nothing but NaN.

## License

Copyright © 2018 Viktor Eikman.

Distributed under the GNU General Public License version 3.0.
