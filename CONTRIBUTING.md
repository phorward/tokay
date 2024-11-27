# Contribution guidelines

Thanks that you want to contribute to Tokay. Contributions of any kind, might it be code, bug reports, bugfixes, documentation, support or advertising are always welcome!

Take a look into the [bug tracker](https://github.com/tokay-lang/tokay/issues) or watch for `//fixme`- and `//todo`-comments in the source code for open issues and things that need to be improved. There are plenty of them.

Feel free to [contact me](https://phorward.info) directly on any questions.

## Issues

In case you encounter a bug, or you miss a feature, please [file an issue](https://github.com/tokay-lang/tokay/issues/new) for that.

## Pull Requests

If you created a solution for a problem or added a feature, any [pull request](https://github.com/tokay-lang/tokay/compare) is welcome!<br>
This can also be made as a draft, in case you want to discuss a change or feature, aren't finished or aren't sure if your change is ready to merge.

In case your pull request is not a draft, ensure that `cargo run` and `cargo test` run without errors. When new features where added, don't miss to write some unit tests for them. Run `cargo fmt` before you finally commit.

## Versioning

Tokay uses the semantic versioning scheme.<br>
Any major/minor/bugfix release is being published to [crates.io](https://crates.io).

As Tokay is unfinished, and under development, it currently has major version 0, with the following versioning convention:

- The minor-version defines huger milestones.
- The bugfix-version is currently not limited to bugfixes only, but might also include smaller milestones between huger milestones, to allow for continous releasing of new features.
- A bugfix version release might not include an updated CHANGELOG.md, even if it contains new features or breaking changes.

As Tokay reaches major version 1, the versioning scheme will be changed into a real semantic versioning, with backward-compatiblity, patch-levels, deprecations etc.

# Maintenance

This section contains some useful information for Tokay maintenance.

## Repository

This repository holds all required source files to provide Tokay with examples.

```
.                  # Cargo.toml, README.md, LICENSE, etc.
├── build          # Build scripts to generate parts of Tokay using Tokay itself
├── examples       # Example programs
├── macros         # Source of the tokay-macros crate required for building
├── src            # Tokay's source code
│   ├── compiler   # Compiler source
│   ├── value      # Object system source
│   └── vm         # Virtual machine source
└── tests          # Part of the test suite with plenty of Tokay test cases
```

## Releasing a version

> The `tokay-macros` crate is located in the folder `macros/`. Preliminarily to a Tokay release, it is necessary to release `tokay-macros` in a separate version to `crates.io`, in case something changed regarding `tokay-macros` during the release. During development, `tokay-macros` can be a local dependency in `Cargo.toml`, but it cannot be a local dependency within a release.

Next are some instructions on how to create a new release.

### Release `tokay-macros`

(only required when version of `tokay-macros` changed)

- `cd macros`
- In `Cargo.toml`, check and/or bump version number
- `cargo publish`

### Release `tokay`

- Make sure `CHANGELOG.md` is updated
- Check for updated copyright year in `LICENSE`, `README.md`, `src/lib.rs` and `src/main.rs`
- Edit `Cargo.toml`
  - Check and/or bump version number
  - Uncomment line to use `tokay-macros` version from crates.io (obviously `tokay-macros = "x.x"`)
  - Comment line to use local `tokay-macros` (should be `# tokay-macros = { version = "x.x", path = "macros" }`)
- Ensure `cargo clean && cargo run && cargo test` succeeds
- `git commit` this state
- `git tag` this state
- `cargo publish`
