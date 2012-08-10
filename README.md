# Cairotrix

A Pajitnovian falling blocks game built on top of Clojure, GTK+ and Cairo.

This is an unfinished work-in-progress. It already features falling blocks and collision detection. It doesn't feature controls (yuck!), sound or a proper "game over".

## Installation

1. Get Java (Sun JDK or OpenJDK)

2. Get [Leiningen](http://github.com/technomancy/leiningen/)

3. Get most of the JAR dependencies using Leiningen:

   ```shell
   $ cd /path/to/cairotrix
   $ lein deps
   ```

4. Get [java-gnome](http://java-gnome.sourceforge.net/get/) (4.1 or higher). On Debian/Ubuntu Linux, I usually do:

   ```shell
   $ sudo aptitude install libjava-gnome-java
   $ cd /path/to/cairotrix
   $ ln -s /usr/share/java/gtk-4.1.jar lib/gtk-4.1.jar
   ```

## Usage

    $ lein run

## Hacking

I've put this project on hold in favour of working on other things. If you'd like to pick it up and finish it, I'd be happy to provide a helping hand.

## License

Copyright (C) 2011  Daniel Werner

Distributed under the Eclipse Public License, the same as Clojure.
