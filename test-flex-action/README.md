
# Test Flex

This action automates running the [test script found in this repository](https://github.com/fmidue/flex-tasks/blob/main/flex-tasks/tasks/README.md) on Flex tasks in .flex file format.

The action will install, build and cache all necessary components to execute the script. This includes `apt-get` packages, the package database and an optional `tex-live` installation.

If there's anything to report, the action will finally submit a comment containing the report files on the corresponding commit.


## Usage

<!-- start usage -->
```yaml
- uses: fmidue/flex-tasks/.github/actions/test-flex@main
  with:
    # Path from repository root to directory containing tasks.
    #
    # Required
    tasks: ''

    # Path from repository root to directory containing package database setup.
    # Needs to contain a `stack.yaml` and `package.yaml` file.
    # See below section `Configuring the package database` for an example.
    #
    # Required
    config: ''

    # Path from repository root to directory containing settings generators for your tasks.
    # see the Readme of the test script for more information.
    #
    # Required
    settings-gens: ''

    # Indicate whether a tex-live distribution should be installed and cached.
    # This is necessary for some tasks using LaTeX formula rendering.
    #
    # Default: false
    latex-setup: ''

    # A glob pattern for matching the location of all Haskell packages in your repository.
    # (the folder where each package.yaml is located)
    # Can be omitted if your repository only contains one package.
    #
    # E.g. with two packages in folders `repo-root/first-pkg` and `repo-root/second-pkg`
    # you can pass pattern '*' to indicate `package.yaml`s can be found one directory deep,
    # instead of the usual location in the root directory.
    #
    # Default: '.' (root directory)
    package-locations: ''

    # A list of sub-directories containing a package.yaml each.
    # This is again only necessary to include if your repository contains multiple packages.
    #
    # Example: '[first-pkg, second-pkg]'
    #
    # Default: '[.]' (only one package in the root directory)
    subdirs: ''

    # The commit SHA of the calling repository to use in building the package db.
    # Will use the SHA contained in the payload of a dispatch event by default.
    # (e.g. event is triggered by a `Compile and Test Haskell Code` workflow)
    #
    # Passing ${{ github.sha }} will be required
    # if this action is called directly without the use of an event.
    #
    # Default: ${{ github.event.client_payload.sha }}
    commit: ''

    # An SSH private key to use while building the package database.
    # This is generally not required,
    # but is necessary if a private repository is included in the database.
    # This includes the repository triggering this action.
    # More information on how to set this up below.
    #
    # Default: ''
    ssh-key: ''

    # Another SSH private key to use while building the package database.
    # This is currently necessary for exactly one user of this action
    # pulling two private repositories for the package database.
    #
    # This will be combined into a list style input with `ssh-key`
    # if more keys are required in the future.
    #
    # Default: ''
    ssh-key2: ''

```
<!-- end usage -->


## Configuring the package database

The package database needs a `stack.yaml` and `package.yaml` to build successfully.
The repository using this action will always be inserted into the `stack.yaml` file with the calling commit. Its dependencies (listed under `extra-deps` in the project's `stack.yaml`) will also be copied over.

All required packages must still be listed in the `package.yaml` even if they can be omitted from the `stack.yaml`.

You can use the minimal files below and add to them as needed:


### stack.yaml

```yaml
---
resolver: nightly-2024-03-20

packages:
  - .

extra-deps:
  - git: https://github.com/fmidue/flex-tasks.git
    commit: b02e270db57bac72471a4732bbb2e87a1fb76d6d
    subdirs:
      - flex-tasks
      - flex-tasks-processing

  - git: https://github.com/fmidue/output-blocks.git
    commit: e97c5f06a17c5b99f0bed2a733bfbf974304adf3
    subdirs:
      - output-blocks

  - git: https://github.com/fmidue/autotool-capabilities.git
    commit: abf9b15ad431e517ae0700ad9dfb6f6941e10538
    subdirs:
      - autotool-capabilities
      - autotool-capabilities-io-instances

  - latex-svg-image-0.2
  - call-alloy-0.6.0.2
  - diagrams-graphviz-1.4.1.1

allow-newer: true
allow-newer-deps:
  - latex-svg-image

```

Note that you can remove any dependency listed here that also is a dependency in your project's `stack.yaml`


### package.yaml

```yaml
---
name: haskell-template-setup
version: 0.1.0.0

extra-source-files:
  - README.md
  - ChangeLog.md

dependencies:
  - autotool-capabilities
  - autotool-capabilities-io-instances
  - base >= 4.7 && < 5
  - flex-tasks
  - output-blocks
  - parsec
  - string-interpolate
  - syb
  - yesod-form

library:
  source-dirs: .

```

Add your project's packages to the dependencies list.


## Including private repositories into the package database

For private repositories you will need to use SSH keys. Follow these steps:

  1. Generate a key pair __without password protection__
  2. Configure the public key as a deploy-key in your private repository.
  3. Configure the private key as an action secret for the repository calling this action.
  4. Pass the secret to this action's input _ssh-key_.

Note that the repositories in steps 2 and 3 could be the same.


## Recommended permissions

When using the `test-flex` action in your workflow, it is recommended to set the following `GITHUB_TOKEN` permissions to ensure proper functionality:

```yaml

permissions:
  contents: write

```
