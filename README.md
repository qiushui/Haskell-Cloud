Haskell Cloud
=============

Haskell Cloud is an [OpenShift](https://www.openshift.com/) cartridge for deploying Haskell on Red Hat's PaaS cloud. It includes:

- GHC 7.6.3
- cabal-install 1.16.0.2
- network 2.4.1.2, and its dependencies

Installation
------------
Use the [Cartridge Reflector](http://cartreflect-claytondev.rhcloud.com/) to obtain the cartridge's manifest, e.g. http://cartreflect-claytondev.rhcloud.com/reflect?github=accursoft/Haskell-Cloud. Alternatively, to create a new Haskell application in OpenShift online, just follow [this link](https://openshift.redhat.com/app/console/application_type/custom?cartridges[]=http://cartreflect-claytondev.rhcloud.com/reflect?github=accursoft/Haskell-Cloud).

Haskell
-------
The application's `cabal` file must define an executable called `server`, which takes two command line arguments; the IP address and port number to listen on. (These can also be take from `$OPENSHIFT_HASKELL_IP` and `$OPENSHIFT_HASKELL_PORT`.) When new code is pushed to the application's repository, the cartridge will build it with `cabal install`, then start the server. The server will be sent the `SIGTERM` signal when the cartridge receives the stop command.

Logging
-------
The logs directory is defined in `$OPENSHIFT_HASKELL_LOG_DIR`. Cabal's build summaries are logged to `build.log`. The application developer is responsible for any other logging - stdout and stderr are not preserved.

Tidying
-------
OpenShift's `tidy` command will delete all logs, cabal's cache of downloaded packages, and the repository working directory. Installed packages (and binaries) are not deleted.
