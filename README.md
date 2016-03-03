``rodeo`` - A code generator for ODE-based models in R
======================================================

Basic facts
---------------------------------------------

``rodeo`` is an add-on package for the [``R ``](https://www.r-project.org/) statistical computing software. It facilitates the implementation of mechanistic models built on ordinary differential equations (ODE).

- ``rodeo`` imposes a well-known standard notation for simultaneous ODE based on the [stoichiometry matrix](http://en.wikipedia.org/wiki/Petersen_matrix). This notation is widely used in chemical and wastewater engineering or biogeoscience.

- ``rodeo`` separates the conceptual model (the set of ODE) from its implementation (computer program) by means of automatic code generation. This makes models more easy (1) to understand by non-programmers, (2) to run on different platforms/environments, (3) to develop or modify, and (4) to 'archive' in times of rapidly changing software.

- The generated code (Fortran, R) can be used with existing suites of numerical solvers, namely the R-packages [``deSolve``](https://cran.r-project.org/web/packages/deSolve/index.html) for dynamic simulation and [``rootSolve``](https://cran.r-project.org/web/packages/rootSolve/index.html) for steady-state estimation. The use of compiled Fortran speeds up numerical integration significantly compared to a purely R-based model implementation (often by a factor between 2 and 100). 

- ``rodeo`` has with built-in support for 1-D partial differential equations (PDE) being tackled with the numerical [method-of-lines](https://en.wikipedia.org/wiki/Method_of_lines).

Package contents
---------------------------------------------

The package currently provides a single reference class named just like the package (``rodeo``). It contains several class- and non-class methods to
  
  - import a conceptual model from tabular text files (or spreadsheets).
  - generate source code to be passed to an ODE-solver.
  - visualize and export basic information about a model, e.g. for documentation purposes.

The [package vignette](https://github.com/dkneis/rodeo/blob/master/vignettes/rodeo.pdf) provides  further details.

Requirements and installation
---------------------------------------------

A current installation of R is required including the tools to build add-on packages from source (Fortran compiler, make utilities, etc.). These tools are typically available by default on Linux systems. Users of other systems should have a look at the [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

The package can be installed directly from its github repository as follows:

```
# install devtools first, if necessary
if (! "devtools" %in% installed.packages()[,"Package"])
  install.packages("devtools")

# now install and load rodeo
library("devtools")
install_github("dkneis/rodeo")
library("rodeo")
```
