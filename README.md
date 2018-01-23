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
applications using stack:

    cd AmbiguityGenerator
    stack build

If you want to install the applications locally you can also use:

    stack install
    
By default this will install the packages in `~/.local/bin`, so you
will need to add that to your PATH.

If you just use `stack build` you can run the programs using `stack
exec`, which we'll use in the following examples.

# Histogram and Graph Generation

There's an application for generating graphs of realizations, and
histograms of the draws under finite support. It can be run as
follows:

    stack exec histogram <runs> <samples> <range> <output-file>
    
- `<runs>` should be replaced with the number of simulations you wish to run.
- `<samples>` is the number of draws from the ambiguity generator.
- `<range>` is a number for the range of finite support values. E.g., `10` will be 0 to 9.
- `<output-file>` is the name of the output file. Something like `graphs.svg` will be good.

The code for this is in the `app/` directory.

# Generating Draws

If you just want to generate a bunch of CSV files of ambiguous data,
you can use the `draws` program:

    stack exec draws <runs> <samples> <file-base>
    
- `<runs>` should be replaced with the number of simulations you wish to run.
- `<samples>` is the number of draws from the ambiguity generator.
- `<file-base>` is the string at the start of the output file.

This will generate a number of CSV files in the current directory of the forms:

- `<fileBase><samples>-bits-<run>.csv`: coin flips.
- `<fileBase><samples>-digits-<run>.csv`: values from 0 to 9.
- `<fileBase><samples>-ambiguous-<run>.csv`: raw realizations.

The code for this program is in the `draws/` directory.

# Web Server

There is a web application which allows users to download CSV files of
certain draws. Additionally this server hosts an API that can be used
to get output from the ambiguity generator in a JSON format.

    stack exec ambiguity-server 8080
    
Will run the web server on port 8080. You can then view the website by
going to: http://localhost:8080

The code for the web server is in the `web/` directory.

# Library

The ambiguity generator library is in `src/`.
