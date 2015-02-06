The package currently provides a single reference class named just like the package ('rodeo'). Objects of this class represent ODE-based models (i.e. models consisting of a set of simultaneous differential equations).

For compatibility with the 'rodeo' class, models must be (re)written in a vector notation. A central element of this notation is the stoichiometry matrix (also called Peterson matrix; http://en.wikipedia.org/wiki/Petersen_matrix). This notation is widely used in, e.g., chemical and wastewater engineering or biogeoscience.

The 'rodeo' class provides methods for the following tasks:

  - Models can be imported from JSON-formatted text files.

  - There is a method to generate source code for used with an ODE-solver (e.g. from R-package 'deSolve'). Target languages are currently R and Fortran95. The generated code is applicable to both zero-dimensional problems (e.g. reactions in a stirred tank) and spatially distributed models (multiple control volumes, e.g. reactive transport models).

  - The stoichiometry matrix can be visualized graphically. This helps in getting an overview on the simulated processes, state variables, and interactions.

