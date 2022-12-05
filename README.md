[![Travis CI Build Status](https://travis-ci.com/yoanlcq/vek.svg?branch=master)](https://travis-ci.org/yoanlcq/vek)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/ir0d4pkpkfwv643q/branch/master?svg=true)](https://ci.appveyor.com/project/yoanlcq/vek/branch/master)
[![Documentation](https://docs.rs/vek/badge.svg)](https://docs.rs/vek)
[![Version](https://img.shields.io/crates/v/vek.svg)](https://crates.io/crates/vek)
![MIT/Apache-2.0](https://img.shields.io/badge/License-MIT%2FApache--2.0-blue.svg)
[![Downloads](https://img.shields.io/crates/d/vek.svg)](https://crates.io/crates/vek)

# vek

Generic 2D-3D math swiss army knife for game engines, with SIMD support and focus on convenience.

See [the wiki](https://github.com/yoanlcq/vek/wiki) for an overview, FAQ, guides, and other info.

# Release notes and breaking changes

Please see [the relevant wiki page](https://github.com/yoanlcq/vek/wiki/Release-notes).

# Compiling on `no_std`

Specify the `vek` dependency in `Cargo.toml` like so :

```toml
[dependencies]
vek = { version = "0.10.0", default-features = false, features = ["libm"] }
```


# Contributing

If you plan to work a pull request, please file an issue first so that we can agree on the best way to go and minimize the amount of wasted time. :)

# CONTRIBUTORS

(See also `authors` in `Cargo.toml`!)

- Joshua Barretto (GitHub: [@zesterer](https://github.com/zesterer))
- Sunjay Varma (GitHub : [@sunjay](https://github.com/sunjay))
- Timo Kösters (GitHub : [@timokoesters](https://github.com/timokoesters))
- Imbris (GitHub: [@imberflur](https://github.com/imberflur))
- Lukas Wirth (GitHub: [@veykril](https://github.com/veykril))
- Martin Taibr (GitHub: [@martin-t](https://github.com/martin-t))
- Patiga (GitHub: [@Patiga](https://github.com/Patiga))
