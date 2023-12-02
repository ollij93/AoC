load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library", "cc_test")

cc_library(
    name = "common",
    hdrs = ["src/common.hpp"],
    includes = ["src"],
)

cc_library(
    name = "day1",
    srcs = ["src/day1/day1.cpp"],
    hdrs = ["src/day1/day1.hpp"],
    deps = [":common"],
)

cc_library(
    name = "day2",
    srcs = ["src/day2/day2.cpp"],
    hdrs = ["src/day2/day2.hpp"],
    deps = [":common"],
)

cc_binary(
    name = "aoc2023",
    srcs = ["src/main.cpp"],
    deps = [
        ":day1",
        ":day2",
    ],
)

cc_test(
    name = "aoc2023_test",
    srcs = ["test/test.cpp"],
    data = [
        "src/day1/day1.testinput1",
        "src/day1/day1.testinput2",
        "src/day2/day2.testinput",
    ],
    deps = [
        ":day1",
        ":day2",
        "@catch2//:catch2_main",
    ],
)
