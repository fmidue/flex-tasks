# Test Script

Use the  `test.sh` script to develop your task configurations.
This will interpret the individual files in GHC and run HLint on them.
A folder named after the task will be created, containing all of the .hs files and two HTML reports.

## Usage

`./test.sh TASK_PATH PKG_DB_PATH`

### example

`./test.sh proplogic.txt flex-pkgdb/pkgdb`

## Notes

- Generate a package database using [our tool](https://github.com/fmidue/haskell-template-setup)
- Both `TASK_PATH` and `PKG_DB_PATH` are given as relative paths (have to be in this folder)
