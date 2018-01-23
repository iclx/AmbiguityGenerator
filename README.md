# Ambiguity

[![Build Status](https://travis-ci.org/HaskellAmbiguity/AmbiguityGenerator.svg?branch=master)](https://travis-ci.org/HaskellAmbiguity/AmbiguityGenerator)

This is a Haskell library to expose the "ambiguous random value generator". The mathematical and practical properties of this generator are available in our paper which you can find here: https://doi.org/10.1287/mnsc.1100.1307

We would be grateful if you would cite us in work that uses this ambiguity generator. We have provided the BibTeX infno here:

```
@article{AmbiguityInTheLabMS2011,
author = {Jack Stecher and Timothy Shields and John Dickhaut},
title = {Generating Ambiguity in the Laboratory},
journal = {Management Science},
volume = {57},
number = {4},
pages = {705-712},
year = {2011},
doi = {10.1287/mnsc.1100.1307},

URL = {
        https://doi.org/10.1287/mnsc.1100.1307
   
},
eprint = {
        https://doi.org/10.1287/mnsc.1100.1307
   
}
```

this info is also available in the `ambiguity.bib` file, if you prefer.

# Installation

You can install the AmbiguityGenerator using the source code. You will
need Haskell for the install. We recommend using
[stack](https://docs.haskellstack.org/en/stable/README/) which will be
able to fetch the Haskell compiler for you.

After installing stack you should fetch the source code:

    git clone https://github.com/HaskellAmbiguity/AmbiguityGenerator.git

Once you have the source code, enter the directory and build the
program using stack:

    cd AmbiguityGenerator
    stack build

