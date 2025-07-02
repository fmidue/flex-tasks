# Test Script

Use the test.sh script to develop your task configurations.
It interprets the individual files in GHC and runs HLint on them.
A matching settings generator has to be supplied for your task (also see below section).
In addition, the script interpolates the `Check.hs` module and scans it for optimization opportunities.

If any suggestions are found, a folder named after your task is created.
This folder contains sub-folders for each settings combination with matching modified `Check.hs` modules and HTML summaries of the three phases described above.
If there are no suggestions, the folder won't be created.

## Usage

`./test.sh TASK_PATH PKG_DB_PATH TASK_SETTINGS_GENERATOR_PATH [-c]`

### example

`./test.sh proplogic.txt flex-pkgdb settings-gens/proplogic.hs`

## Requirements

- stack
- ghci version used in the package database (if you don't know, execute the script and look for an error message)
- expect
- ansi2html (colorized-logs)

## Settings Generators

Use an external generator to produce different variations of your task's settings for testing.
This Haskell file need to satisfy the following:

- module name is `SettingsGen`
- contains type definition for a record type `Settings`
- this type's fields are named exactly as your configuration's options (located in TaskSettings)
- the type derives `Show`
- it contains a function `rollSettings :: Gen Settings` that generates a value of your `Settings` type

## Notes

- Generate a package database using [our tool](https://github.com/fmidue/haskell-template-setup)
- Both `TASK_PATH` and `PKG_DB_PATH` are given as relative paths (have to be in this folder)
- The `-c` flag can optionally be supplied to preserve the generated `Check.hs` file even if no reports were generated
