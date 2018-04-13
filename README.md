# multigit

```
g421 - git utilities for CS 421

Usage: mg COMMAND
  Manipulate class git repositories

Available options:
  -h,--help                Show this help text

Available commands:
  check                    Check repository
  distribute               Distribute/update files
  add                      Add a path to repositories
  commit                   Commit contents of repositories
  push                     Pull --rebase
  pull                     push repositories
  pullpush                 Pull --rebase then push repositories
  rm                       Remove a file
```

Typically you will run the command with the regular git commands,
followed by a list of all the repositories you want to act upon.

Examples:
  - To distribute the files in ~/tmp/04-12 to examples/04-12 in each repo:
    `mg distribute ~/tmp/04-12 examples/04-12 "Examples from 04-12" *`
  - To sanity check the repos
    `mg check *`
  - To push changes to the server
    `mg pullpush *`

etc...
