# Contributing to tree-sitter-erlang

We want to make contributing to this project as easy and transparent as possible.

## Our Development Process

tree-sitter-erlang is currently developed in Meta's internal repositories and then exported
out to GitHub by a Meta team member; however, we invite you to submit pull
requests as described below.

## Pull Requests

We actively welcome your pull requests.

1. Fork the repo and create your branch from `main`.
2. If you've added code that should be tested, add tests.
3. Ensure the test suite passes.
4. If you haven't already, complete the Contributor License Agreement ("CLA").

### Grammar Validation

This grammar is used as the front end for the [Erlang Language Platform (ELP)](https://github.com/WhatsApp/erlang-language-platform).

As part of this process, we generate an ELP AST structure from the generated `grammar.json` file.

So a pull request will not be landed until it has matching changes in ELP.

The most important thing is to check that the code generation does not
fail, as it imposes additional constraints on the shape of the grammar.

Thes can be checked by

1. Setting up an ELP development environment according it [CONTRIBUTING](https://github.com/WhatsApp/erlang-language-platform/blob/main/CONTRIBUTING.md)

2. Updating
   [Cargo.toml](https://github.com/WhatsApp/erlang-language-platform/blob/c4d3602e08418432a020abb734723727a8374549/Cargo.toml#L94)
   to get the `tree-sitter-erlang` crate from your checked out changed `tree-sitter-erlang` repository.

3. In the `ELP` repository, running `cargo xtask codegen`.  If it succeeds
   without errors, the grammar change is acceptable.

4. For bonus points you can then `cargo build` ELP and deal with
   changes induced by the grammar, but that is not necessary.
   However, the change will not land until a matching change is made
   to ELP by a contributor.

## Contributor License Agreement ("CLA")

In order to accept your pull request, we need you to submit a CLA. You only need
to do this once to work on any of Facebook's open source projects.

Complete your CLA here: <https://code.facebook.com/cla>

## Issues

We use GitHub issues to track public bugs. Please ensure your description is
clear and has sufficient instructions to be able to reproduce the issue.

## License

By contributing to tree-sitter-erlang, you agree that your contributions will be
licensed under the [LICENSE](LICENSE) in the root directory of this source tree.
