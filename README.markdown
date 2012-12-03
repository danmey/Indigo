# Installing
Indigo is distributed as an OPAM package.

Please refer to the OPAM website how to install the package manager.

## Installing Indigo
    $ opam switch 4.00.1
    $ eval `opam config -env`
    $ opam repo -add indigo https://github.com/danmey/Indigo.git
    $ opam update
    $ opam install indigo-0

# Server
## BOLT configuration
To run server with logging `BOLT_FILE=log-config ./server.native`

To use sample config `cp log-config.sample log-config`.