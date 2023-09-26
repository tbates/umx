## OS X installation

1. Install Xcode from the App Store
2. Run `xcode-select --install` in a terminal window to get standard C libraries etc
  * *note*: app store will update it thereafter.
3. If the RMPI package fails to build with configure: error: "Cannot find mpi.h header file", then `brew install mpi` should get you on the path to have rmpi working.
4. Clone OpenMx into `~/bin/OpenMx` (or whatever you prefer, but this is standard)
  * `git clone git://github.com/OpenMx/OpenMx.git`
  * cd OpenMx
5. Download and Install latest R and OpenMx's required packages
6. Add (or create) the following to `~/.R/Makevars`

CC = /usr/local/bin/gcc
CXX = /usr/local/bin/g++
CXX1X = /usr/local/bin/g++
FC = /usr/local/bin/gfortran
F77 = /usr/local/bin/gfortran

Important get the correct version of NPSOL for your build.
You will need to find the right right version of libnpsol.a (one copy is appended at the bottom of this wiki page).
This must be moved to the correct location for the compiler to find it. By default this will be trunk/inst/npsol/osx/

make install
Now you can make (compile) and install (move the package in R's library folder) OpenMx.
Again, from the top directory, simply run make install Other options for building (like check) are described in full in the unix section above and work for Mac too.

Validate the installation by running make test - it takes some time to complete.

The above steps have been tested on a newly (Aug 5 2014) purchased OS X 10.9.2 system. For subsequent rebuilds from source the procedure is much easier:

cd OpenMx
git fetch origin
git reset --hard origin/master
make install
