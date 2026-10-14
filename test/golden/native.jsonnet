local concat = std.native("concat");
local add = std.native("add");
local nonexistent = std.native("nonexistent");

{
  string_test: concat("hello ", "world"),
  number_test: add(20, 22),
  missing_func_is_null: nonexistent == null,
  curried_test: std.map(function(x) add(10, x), [1, 2, 3]),
}
