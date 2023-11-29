# Advent of Code solutions

## 2023 - C/C++

C/C++ built with bazel

Build with:

```bash
$ bazel build //2023:aoc2023
```

Run with:

```bash
$ bazel-bin/2023/aoc2023
Hello, World!
```

### Debugging

Add the `--compilation_mode=dbg` flag to the build command and load the binary with lldb:

```bash
 $ lldb bazel-bin/2023/aoc2023
(lldb) target create "bazel-bin/2023/aoc2023"
Current executable set to '/Users/olijohns/AoC/bazel-bin/2023/aoc2023' (arm64).
(lldb) process launch --stop-at-entry
...
```

This is setup in the launch.json and task.json files for VSCode to enable interactive debugging in the editor.

## 2022 - haskell

My first haskell project

[View benchmark report](https://htmlpreview.github.io/?https://github.com/ollij93/AoC/blob/main/2022/bench.html)

## 2021 - rust

My first rust!

Advent of Code solutions for 2021 written in rust.
My first time using rust so a lot of this is probably very questionable!

The inputs for each day are stored in the `inputs/` directory, along with some
test examples from the problem descriptions.

The code is stored in `src/dayX.rs` where each day has a public `run()`
function which get invoked from `main()` in `src/main.rs`. To run the code
for a given day specify the CLI option for the day and provide the input on
stdin like so:

```bash
$ cargo run -- -d 1 < inputs/day1.txt
```
