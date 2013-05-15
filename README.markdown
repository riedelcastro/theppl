theppl
==============

[![Build Status](https://travis-ci.org/riedelcastro/theppl.png)](https://travis-ci.org/riedelcastro/theppl)
==============

theppl (pronounced "the-people") is a (p)robabilistic (p)rogramming (l)anguage/platform. It is designed to overcome
the fundamental problem with probabilistic programming: the difficulty of scaling it up to real world
applications. 

To achieve this, it relies on modularity:
Developers can hierarchically compose modules (aka templates) into larger modules. Submodules can be designed in
declarative ways (say, through MLNs), tailor-made (say, a parser using a dynamic program) or
downloaded from other module providers. 

Modules can be orchestrated in various ways: pipelines, as joint potentials, in isolation etc. 

Modularity allows developers to 
    (a) inject specific inference know-how if needed, 
    (b) design and test their system in a divide-and-conquer manner, and 
    (c) reuse existing components.

Install
--------
theppl uses sbt for building and installation. If you don't have sbt on your machine,
get it [here](https://github.com/harrah/xsbt). Then run:

    sbt publish-local

Now the theppl library is installed locally and can be used by adding dependency to your sbt/maven builds.


