# Project Status
[![Build Status](https://travis-ci.org/laurajuliamelis/Lab6.svg?branch=master)](https://travis-ci.org/laurajuliamelis/Lab6)

# Lab 6: knapsack

Intermediate API package for the Swedish Parliament Votations.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

The package require a basic installation of R. In addition devtools and testthat packages.

```
install.packages("devtools")
install.packages("testthat")
```

### Installing

Run the following in R:

```
devtools::install_github("laurajuliamelis/Lab6", subdir="knapsack", build_vignettes=TRUE)
```

## Running the tests

```
devtools::test()
```

### Shiny Visualisations

This project has a Shiny application. Run the following to start Shiny:

```
runGitHub("laurajuliamelis/Lab5Shiny", subdir = "ShinySwedishParliamentVotations")
```

## Built With

* [Travis CI](https://travis-ci.org)


## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Friends and family for making this possible.