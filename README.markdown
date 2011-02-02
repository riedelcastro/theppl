theppl
==============

theppl is a (p)robabilistic (p)rogramming (l)anguage/platform.

Install
--------
theppl uses maven for building and installation. If you don't have maven on your machine,
get it [here](http://maven.apache.org/). Then run:

    mvn install

Create Own Project
------------------
The easiest way is to create a theppl maven project through

    mvn archetype:create                                   \
     -DarchetypeGroupId=com.github.riedelcastro.theppl     \
     -DarchetypeArtifactId=theppl-archetype                \
     -DarchetypeVersion=1.0-SNAPSHOT                       \
     -DgroupId=<yourgroupid>                               \
     -DartifactId=<yourprojectid>

This will create a simple example maven project you can extend. Note that you can use
your favourite IDE to import the pom file, and then work within this IDE on
your theppl project.