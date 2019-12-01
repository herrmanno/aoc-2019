module main2;

import std.stdio;
import std.file;
import std.array;
import std.algorithm;
import std.conv;
import std.math;
import std.format;

private float fuel(float n) {
  auto m = floor(n / 3f) - 2;
  if (m <= 0) {
    return 0;
  } else {
    return m + fuel(m);
  }
}

void main() {
  auto sum = readText("input.txt").split("\n")
    .map!(a => to!int(a))
    .map!(a => fuel(a))
    .reduce!"a + b";

    writeln(format("%d", to!int(sum)));
}