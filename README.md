# Flex-Tasks

Runtime interpreted flexible exercises, mainly for use in [Autotool](https://gitlab.imn.htwk-leipzig.de/autotool/all0).
Also automatic HTML form and parser generation for input data types.


## Contents

The repository includes two packages.
Directory `tasks` includes a collection of sample tasks and a test script.

[Haddock Documentation](https://fmidue.github.io/flex-tasks/)

### flex-task

Supplies both a way to run tasks, as well as utilities to simplify the creation of said tasks.
* `FlexTask.Types` defines task configurations and instances.
* `FlexTask.Interpreter` contains functions to evaluate such tasks at runtime.
* The remaining modules implement various utility functions and automations for creating tasks, for example:
    * `FlexTask.Generic.Form`  provides generic, composable input forms for most data types.
    * `FlexTask.Generic.Parse` does the same for generic parsers.

### flex-task-processing

Includes text processing necessary for the Autotool frontend. It is split of from the above package to minimize dependencies in Autotool.


## Usage

The package is incorporated into Autotool. The task evaluation is currently not intended to be used on its own.
In Autotool, you can select the task type `Flex` to input a task configuration. A default is given, which can be edited or completely overwritten by one of the sample tasks in this repository (or your own).
For task development, consider using the provided test script in `tasks`. Please refer to its own readme file.

The generic parsers and input forms can be used as a standalone feature, independently from Autotool.


## Running tests

The test suite for flex-task needs a specified external package database. Its path has to be set via the environment variable `FLEX_PKGDB` before running the tests. You can use the default database of the package by setting `FLEX_PKGDB` to `$(stack path --local-pkg-db)`.
