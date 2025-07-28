<!-- omit in toc -->
# Simple Nix Starter Project
This starter project contains the scaffolding needed to integrate Clash with the Nix Stack build system. Read [Simple Starter Project](https://github.com/clash-lang/clash-starters/blob/main/simple/README.md) for more information on the various files.

<!-- omit in toc -->
# Table of Contents
- [Getting this project](#getting-this-project)
- [Building and testing this project](#building-and-testing-this-project)
- [REPL](#repl)
- [Adding custom dependencies / updating nix](#adding-custom-dependencies--updating-nix)

# Getting this project
Run `nix-shell --packages stack --run "stack new my-clash-project clash-lang/simple-nix"`.

# Building and testing this project
Build the project with:

```bash
nix-build
```

Verilog code will be available under the `result/share/verilog` directory.
Modify the `hdl` variable in `default.nix` to configure whether to generate
SystemVerilog or VHDL.

However development itself is more streamlined by using a Nix shell. Start one
by invoking:

```bash
nix-shell
```

Then, to run the tests defined in `tests/`:

To compile the project to VHDL run:

```bash
cabal run clash -- Example.Project --vhdl
```

You can find the HDL files in `vhdl/`. The source can be found in `src/Example/Project.hs`.

# REPL

```bash
clashi -isrc
```

and then, into the clashi repl, with the import path `src`

```bash
>>>:l Chap4_6.Project
>>>:verilog
```

# Adding custom dependencies / updating nix
`niv` is available after opening `nix-shell`. See [niv on Hackage](https://hackage.haskell.org/package/niv) for more information.

[retroclash-lib](https://hackage.haskell.org/package/retroclash-lib)
