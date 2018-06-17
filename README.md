# DMOTE wrist rest topology generator

This is a CLI script which will output a three-dimensional topology in the
simple format expected by OpenSCAD.

It is intended to be used with the DMOTE keyboard project, specfically in
generating the surface of a wrist rest.

Because a wrist rest should have fairly smooth organic curves, the topology
is based on a [bivariate normal distribution function](https://en.wikipedia.org/wiki/Multivariate_normal_distribution).

## Usage

For help with the parameters:

    $ lein run -- -h

## License

Copyright © 2018 Viktor Eikman.

Distributed under the GNU General Public License version 3.0.
