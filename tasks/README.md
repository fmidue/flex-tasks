# Test Script

Use the  `test.sh` script to develop your task configurations.
This will interpret the individual files in GHC and run HLint on them.
A folder named after the task will be created, containing all of the .hs files and two HTML reports. 

## Usage

`./test.sh TASK_PATH PKG_DB_PATH GHC_VERSION`

### example:

`./test.sh proplogic.txt flex-pkgdb/pkgdb 9.8.2`

## Notes

- Generate a package database using [our tool](https://github.com/fmidue/haskell-template-setup)
- Both `TASK_PATH` and `PKG_DB_PATH` are given as relative paths (have to be in this folder)
- `GHC_VERSION` has to match the version of the package database.
  This is determined by the used resolver inside the stack.yaml when building the database.
