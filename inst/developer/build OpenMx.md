# OS X installation

## Make and install OpenMx.
If you've installed R, Xcode and Fortran (see 2 below), you are ready to make and install OpenMx.

1. In `terminal.app`, cd to `~/bin/OpenMx/`

2. `make install`
3. optionally `make test` to validate (takes time)

You can also do this from inside R
> 
devtools::build("~/bin/OpenMx", binary = TRUE)
system(paste0("open ", .libPaths()[1]))

## Setup: Installing needed software
1. Install R and OpenMx's required packages.
2. Install `Xcode` (on the App Store)
4. Download and install `gfortran-nn.n-universal.pkg` from [https://cran.r-project.org](https://cran.r-project.org/bin/macosx/tools/)
5. Clone OpenMx into `~/bin/OpenMx` (anywhere's fine, but this is standard)

```
git clone git://github.com/OpenMx/OpenMx.git
cd OpenMx
```
### to get a particular tag

```
git fetch origin --tags
git checkout v2.21.8
```

6. Add (or create) the following to `mate ~/.R/Makevars`
>
CC    = /usr/bin/gcc
CXX   = /usr/bin/g++
CXX1X = /usr/bin/g++
FC    = /opt/gfortran/bin/gfortran
F77   = /opt/gfortran/bin/gfortran

7. Install OpenMP
 * https://mac.r-project.org/openmp/
 * Add the following to ~/.R/Makevars

```
CPPFLAGS += -Xclang -fopenmp
LDFLAGS  += -lomp
```

*Notes: Usually not necessary now*

1. If the RMPI package fails to build with configure: error: "Cannot find mpi.h header file", then it will need to be installed. This used to be via a brew instruction: `brew install mpi`
2. Run `xcode-select --install` in a terminal window to get standard C libraries etc
  * *note*: app store will update it thereafter.
8. You need the correct version of NPSOL for your build. It likely came inside the git repo at "~/bin/OpenMx/inst/npsol/osx/x86_64/libnpsol.a"
	* If not, A copy might be available at the bottom of [https://openmx.ssri.psu.edu/wiki/howto-build-openmx-source-repository](https://openmx.ssri.psu.edu/wiki/howto-build-openmx-source-repository) which you would have to download, rename, and move into position.  

