``rodeo`` - Code generator for ODE-models in R
======================================================

Basic facts
---------------------------------------------

``rodeo`` is an add-on package for the [``R ``](https://www.r-project.org/) statistical computing software. It facilitates the implementation of mechanistic models built on ordinary differential equations (ODE).

- ``rodeo`` imposes a well-known standard notation for simultaneous ODE based on the [stoichiometry matrix](https://en.wikipedia.org/wiki/Petersen_matrix). This notation is widely used in chemical and wastewater engineering or biogeoscience.

- ``rodeo`` separates the conceptual model (the set of ODE) from its implementation (computer program) by means of automatic code generation. This makes models more easy (1) to understand by non-programmers, (2) to run on different platforms/environments, (3) to develop or modify, and (4) to 'archive' in times of rapidly changing software.

- The generated code (Fortran, R) can be used with existing suites of numerical solvers, namely the R-packages [``deSolve``](https://cran.r-project.org/package=deSolve) for dynamic simulation and [``rootSolve``](https://cran.r-project.org/package=rootSolve) for steady-state estimation. The use of compiled Fortran speeds up numerical integration significantly compared to a purely R-based model implementation (often by a factor between 2 and 100). 

- ``rodeo`` has with built-in support for 1-D partial differential equations (PDE) being tackled with the numerical [method-of-lines](https://en.wikipedia.org/wiki/Method_of_lines).

Package contents
---------------------------------------------

The package currently provides a single R6 class named just like the package (``rodeo``). It contains several class- and non-class methods to
  
  - import a conceptual model from tabular text files (or spreadsheets).
  - generate source code to be passed to an ODE-solver.
  - visualize and export basic information about a model, e.g. for documentation purposes.
  - generate a stoichiometry matrix from a set of reaction equations.
  - validate a stoichiometry matrix (mass balance check).

The package vignette provides further details.

Requirements and installation
---------------------------------------------

A current installation of R is required. In order to work with ``rodeo`` in high-performance mode, one needs the tools to compile Fortran 2008 code (compiler, make utilities, etc.). These tools are typically available by default on Linux systems. Users of other systems should have a look at the [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

The development version of the package can be installed directly from the github repository as shown below. For this, the ``devtools`` package is required.

```
# Install and load development version
library("devtools")
install_github("dkneis/rodeo")
library("rodeo")
```

The release version should be available on CRAN and can be installed as usual (see below). Note that the release version may lag behind the development version in terms of functionality.

```
# Install and load release version
install.packages("rodeo")
library("rodeo")
```
