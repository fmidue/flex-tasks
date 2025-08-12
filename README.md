# Flex-Tasks

Runtime interpreted flexible exercises, mainly for use in [Autotool](https://git.imn.htwk-leipzig.de/waldmann/autotool).
Also automatic HTML form and parser generation for input data types.


## Contents

This repository includes two packages:

### flex-tasks

Supplies both a way to run tasks and utilities to simplify the creation of said tasks.
* `FlexTask.Types` defines task configurations and instances.
* `FlexTask.Interpreter` contains functions to evaluate such tasks at runtime.
* The remaining modules implement various utility functions and automations for creating tasks, for example:
    * `FlexTask.Generic.Form`  provides generic, composable input forms for most data types.
    * `FlexTask.Generic.Parse` does the same for generic parsers.

### flex-tasks-processing

Includes text processing necessary for the Autotool frontend. It is split off from the above package to minimize dependencies in Autotool.


## Documentation

* [Haddock Documentation](https://fmidue.github.io/flex-tasks/)
* [Wiki](https://github.com/fmidue/flex-tasks/wiki)


## Usage

The package is incorporated into Autotool. The task evaluation is currently not intended to be used on its own.
In Autotool, you can select the task type `Flex` to input a task configuration. A default is given, which can be edited or completely overwritten by your own task.
For task development, consider using the provided test script in `flex-tasks/tasks`. Please refer to its own README file.

The generic parsers and input forms can be used as a standalone feature, independently of Autotool.


## Running local tests

The test suite for flex-tasks needs a specified external package database. Its path has to be set via the environment variable `FLEX_PKGDB` before running the tests. You can use the default database of the package by setting `FLEX_PKGDB` to `$(stack path --local-pkg-db)`.
The test-suite also requires additional dependencies not needed for compiling the library.
To execute the tests use the provided config file `stack-test.yaml` via `stack test --stack-yaml stack-test.yaml` instead.
