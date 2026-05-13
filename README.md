# RAM Machine Simulator

A web-based Random Access Machine (RAM) simulator built with Elm. The app lets you write and run RAM programs directly in the browser, inspect register values, work with input and output tapes, and execute programs either step by step or automatically.

## Demo

[Open GitHub Pages](https://annasikalenko19.github.io/RAM_BP/RAM/)

## Features

- RAM code editor with `localStorage` persistence;
- program import and export as `.txt` files;
- input and output tapes;
- register table from `R0` to `R100`;
- step-by-step execution with current command and changed register highlighting;
- automatic execution with adjustable speed;
- error messages and infinite-loop protection;
- built-in reference for supported commands.

## Supported Commands

`READ`, `WRITE`, `LOAD`, `STORE`, `ADD`, `SUB`, `MUL`, `DIV`, `JUMP`, `JZERO`, `JGTZ`, `HALT`.

The simulator supports direct register addressing, indirect addressing with `*`, and constants with `=`, for example:

```txt
READ 1
LOAD =10
ADD 1
STORE 2
WRITE 2
HALT
```

## Local Development

Requires Elm `0.19.1`.

```bash
cd RAM
elm make src/Main.elm --output=elm.js
```

After building, open `RAM/index.html` in a browser or run any local static server from the `RAM` directory.
