module main1;

import std.stdio;
import std.file;
import std.array;
import std.algorithm;
import std.conv;
import std.math;
import std.format;

void main() {
  auto sum = readText("input.txt").split("\n")
    .map!(a => to!int(a))
    .map!(a => floor(a / 3f) - 2)
    .reduce!"a + b";

    writeln(format("%d", to!int(sum)));
}