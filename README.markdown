Welcome
=======

Indigo is a brand new tabletop engine for playing various games requiring interaction of people over the network.

Please keep in mind that the work here is no near production quality, it's currently just an initial sketch of the functionality.

Contributing
============

Anybody is free to contribute to the Indigo project, and any contributions are very welcome!

Installing
==========
Indigo is distributed as an OPAM package.

Please refer to the OPAM website how to install the package manager.

Dependencies
------------

Indigo currently depends on a few packages. The list of dependencies can be seen inside the OPAM package description.

To install the dependencies and the latest git Indigo (might not build) via OPAM:

    $ opam switch 4.00.1
    $ eval `opam config -env`
    $ opam repo -add indigo https://github.com/danmey/Indigo.git
    $ opam update
    $ opam install indigo-0


Debugging
=========

Server
------
To run server with logging `BOLT_FILE=log-config ./server.native`

To use sample config `cp log-config.sample log-config`.
