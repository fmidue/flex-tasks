# Flex-Tasks

Runtime interpreted flexible exercises, mainly for use in [Autotool](https://git.imn.htwk-leipzig.de/waldmann/autotool).
Also automatic HTML form and parser generation for input data types.


## Contents

This repository includes two packages, some sample tasks and a GitHub action to validate task configurations via CI.

### flex-tasks

Supplies both a way to run tasks and utilities to simplify the creation of said tasks.
* `FlexTask.Config.Types` defines task configurations and instances.
* `FlexTask.Interpreter` contains functions to evaluate such tasks at runtime.
* The remaining modules implement various utility functions and automations for creating tasks, for example:
    * `FlexTask.Form`  provides generic, composable input forms for most data types.
    * `FlexTask.Parser` does the same for generic parsers.

### flex-tasks-processing

Includes text processing necessary for the Autotool frontend. It is split off from the above package to minimize dependencies in Autotool.

### Sample task configurations

* [Default configuration](flex-tasks/tasks/defaultConfig.flex) (includes a manual)
* [Seat distributor for exams](flex-tasks/tasks/examSeating.flex)
* [Some old tasks](flex-tasks/tasks/interpolation-test) used for runtime experiments.
  Their contents aren't very interesting, but they outline what a minimal task skeleton looks like.

### Test-Flex action

You can validate your task configurations via GitHub CI using our action. Refer to the [action's README](test-flex-action/README.md) for setup.

## Documentation

* [Haddock Documentation](https://fmidue.github.io/flex-tasks/)
* [Wiki](https://github.com/fmidue/flex-tasks/wiki)


## Usage

The package is incorporated into Autotool. The task evaluation is currently not intended to be used on its own.
In Autotool, you can select the task type `Flex` to input a task configuration. A default is given, which can be edited or completely overwritten by your own task.
For task development, consider using the provided test script in [test-flex-action/components](test-flex-action/components/test.sh). Please refer to its own [README file](test-flex-action/components/README.md).

The generic parsers and input forms can be used as a standalone feature, independently of Autotool.


## Running local tests

The test suite for flex-tasks needs a specified external package database. Its path has to be set via the environment variable `FLEX_PKGDB` before running the tests. You can use the default database of the package by setting `FLEX_PKGDB` to `$(stack path --local-pkg-db)`.
