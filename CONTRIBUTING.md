# Preliminaries
 
Before contributing to any Typed Clojure asset, please read the [Typed Clojure Contributor Code of Conduct](CODE_OF_CONDUCT.md).

All contributors agree to submit their code under the terms of the [Clojure Contributors Agreement](https://clojure.org/dev/contributor_agreement).
Please submit a Clojure CA form by following the links on that page.

## Pull requests

All pull requests should have an associated [issue](https://github.com/typedclojure/typedclojure/issues).
Please describe the change you'd like to have at a high level in the issue,
and then open a pull request to discuss implementation details.

## Verifying code blocks in documentation

When updating documentation in `website/docs/`, code blocks can be automatically tested with the bidirectional documentation sync system. See [BIDIRECTIONAL_DOC_TESTING.md](BIDIRECTIONAL_DOC_TESTING.md) for details on:

- Adding code blocks that will be automatically type-checked
- Syncing changes between documentation and test files
- Capturing type errors from the type checker
- Running documentation tests with `./script/run-doc-tests.clj`

## Ideas

If you have an idea that you would like to discuss, you can either:

- open an issue on [typedclojure](https://github.com/typedclojure/typedclojure/issues)
- post it on the [Typed Clojure mailing list](https://groups.google.com/forum/#!topic/clojure-core-typed)
