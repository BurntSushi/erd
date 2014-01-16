This utility takes a plain text description of entities, their attributes and 
the relationships between entities and produces a visual diagram modeling the 
description. The visualization is produced by using Dot with GraphViz. There 
are limited options for specifying color and font information. Also, `erd` can
output graphs in a variety of formats, including but not limited to: pdf, svg, 
eps, png, jpg, plain text and dot.


### Installation

[erd is on hackage](http://burntsushi.net/docs/haddock/erd), so you can install 
it with cabal:

    cabal install erd

Alternatively, you can clone this repository and build from source:

    git clone git://github.com/BurntSushi/erd
    cd erd
    cabal configure
    cabal build
    # binary is now at ./dist/build/erd/erc

Usage information is available with `erd --help`.

### Quick example

Before describing the ER file, let's try making an ER diagram from a small 
example:

```bash
$ curl 'https://raw.github.com/BurntSushi/erd/master/examples/simple.er' > simple.er
$ cat simple.er
# Entities are declared in '[' ... ']'. All attributes after the entity header
# up until the end of the file (or the next entity declaration) correspond
# to this entity.
[Person]
*name
height
weight
+birth_location_id

[Location]
*id
city
state
country

# Each relationship must be between exactly two entities, which need not
# be distinct. Each entity in the relationship has exactly one of four
# possible cardinalities:
#
# Cardinality    Syntax
# 0 or 1         0
# exactly 1      1
# 0 or more      *
# 1 or more      +
Person *--1 Location
$ erd -i simple.er -o simple.pdf
```

The PDF should now contain a graph that looks like this:

![Simple erd example graph](http://burntsushi.net/stuff/erd-example-simple.png)

