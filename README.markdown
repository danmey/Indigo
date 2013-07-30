Welcome
=======

Indigo is a brand new tabletop engine for playing various games requiring interaction of people over the network.

Please keep in mind that the work here is no near production quality, it's currently just an initial sketch of the functionality.


Contributing
============

Anybody is free to contribute to the Indigo project, and any contributions are very welcome!

The best way is to fork the project, find a bug or issue that has been reported on the bug tracker:

  https://github.com/danmey/Indigo/issues

fix it or implement it and send the pull request.


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
    $ opam remote -add indigo https://github.com/danmey/Indigo.git
    $ opam update
    $ opam install indigo-0


Manual installation
===================

After installing the dependencies which are:

- ocaml >3.12.1
- lablgtk2
- lwt
- react
- yojson
- bolt

you can use one of these two commands to build either native or bytecode version of the client and the server:

   $ ocamlbuild -use-ocamlfind indigo.native.otarget
   $ ocamlbuild -use-ocamlfind indigo.byte.otarget

Debugging
=========

Server
------
To run server with logging `BOLT_FILE=log-config ./server.native`

To use sample config `cp log-config.sample log-config`.
