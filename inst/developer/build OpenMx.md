## OS X installation

1. Download and Install latest R and OpenMx's required packages
2. Install Xcode from the App Store
3. Run `xcode-select --install` in a terminal window to get standard C libraries etc
  * *note*: app store will update it thereafter.
4. If the RMPI package fails to build with configure: error: "Cannot find mpi.h header file", then `brew install mpi` should get you on the path to have rmpi working.
5. Download and install the `gfortran-nn.n-universal.pkg`  available at: https://cran.r-project.org
6. Clone OpenMx into `~/bin/OpenMx` (or whatever you prefer, but this is standard)
  * `git clone git://github.com/OpenMx/OpenMx.git`
  * cd OpenMx
7. Add (or create) the following to `~/.R/Makevars`

CC    = /usr/bin/gcc
CXX   = /usr/bin/g++
CXX1X = /usr/bin/g++
FC    = /opt/gfortran/bin/gfortran
F77   = /opt/gfortran/bin/gfortran


8. You need the correct version of NPSOL for your build. It likely came inside the git repo at "~/bin/OpenMx/inst/npsol/osx/x86_64/libnpsol.a"

If not, A copy might be available at the bottom of https://openmx.ssri.psu.edu/wiki/howto-build-openmx-source-repository)
Which you would have to rename and move.

9. Install OpenMP
https://mac.r-project.org/openmp/

Now you can make and install OpenMx. From `~/binOpenMx`, run `make install`

Validate the installation by running make test - it takes some time to complete.

For subsequent rebuilds from source the procedure is much easier:

cd ~/bin/OpenMx
git fetch origin
make install

In R
devtools::build("~/bin/OpenMx", binary = TRUE)
system(paste0("open ", .libPaths()[1]))
