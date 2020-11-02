std.manifestJsonEx(
  {
      x: [1, 2, 3, true, false, null,
          @"string\n""string""\n"],
      y: { a: 1, b: 2, c: [1, 2] }
  },
  "  "
)
