theppl
==============

theppl is a (p)robabilistic (p)rogramming (l)anguage/platform. It is designed to overcome
the fundamental problem with probabilistic programming: the difficulty of scaling it up to real world
applications. To achieve this, it relies on modularity:
Developers can hierarchically compose modules into larger modules. Submodules can be designed in
declarative ways (say, through MLNs), tailor-made (say, a parser using a dynamic program) or
downloaded from other module providers. The modules can be orchestrated in various ways: pipelines,
as joint models, in isolation etc. Modularity allows developers to (a) inject specific inference know-how
if needed, (b) design and test their system in a divide-and-conquer manner, and (c) reuse existing
components.

For further details please have a look at the [manual](https://github.com/riedelcastro/riedelcastro-maven-repo/raw/master/snapshots/com/github/riedelcastro/theppl/theppl-manual/0.1.0-SNAPSHOT/theppl-manual-0.1.0-SNAPSHOT.pdf).

Install
--------
theppl uses sbt for building and installation. If you don't have maven on your machine,
get it [here](https://github.com/harrah/xsbt). Then run:

    sbt publish-local

Now the theppl library is installed locally and can be used by adding dependency to your sbt/maven builds.


