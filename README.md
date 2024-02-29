
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Library of statistical methods for chef-style AMNOG analyses

The chefStats package aims to provide a library of fast, validated
methods for use in AMNOG analysis created by the chef package.h

As the functions found in chefStats are designed to be used with chef,
it may be unwieldy to use these functions independently.

# Setup

## Install githooks

This project supports two styles of githooks.

1.  The first style is to use the githooks provided in the `.githooks`
    directory. To use these hooks, run the following command in the root
    of the project:

- These hooks are very simple just blocking the commit to protected
  branches. \`\` git config –local core.hooksPath .githooks/

<!-- -->


    2. The second is to install the precommit tool (for linux) [precommit](https://pre-commit.com/). 
      - These are much more powerful and can be used to run checks on the code before it is committed.

pipx install pre-commit \# Then run in the root of repo: pre-commit
install \`\`\`
