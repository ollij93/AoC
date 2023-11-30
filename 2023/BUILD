load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_test", "cc_library")

cc_library(
    name = "common",
    hdrs = ["src/common.h"],
    includes = ["src"],
)

cc_library(
    name = "day1",
    srcs = ["src/day1/day1.cc"],
    hdrs = ["src/day1/day1.h"],
    deps = [":common"],
)

cc_binary(
    name = "aoc2023",
    srcs = ["src/main.cc"],
    deps = [":day1"],
)

cc_test(
    name = "aoc2023_test",
    srcs = ["test/test.cc"],
    deps = [
        "@catch2//:catch2_main",
        ":day1",
    ],
)
