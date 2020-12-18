/*
Copyright 2015 Google Inc. All rights reserved.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

// This contains some of the tests from google/jsonnet test_suite


std.assertEqual(std.makeArray(10, function(i) i + 1), [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) &&
std.assertEqual(std.makeArray(0, function(i) null), []) &&

local assertClose(a, b) =
  local err =
    if b == 0 then
      a - b
    else
      if a / b - 1 > 0 then a / b - 1 else 1 - a / b;
  if err > 0.000005 then
    error 'Assertion failed (error ' + err + '). ' + a + ' !~ ' + b
  else
    true;

std.assertEqual(std.pow(3, 2), 9) &&
std.assertEqual(std.floor(10), 10) &&
std.assertEqual(std.floor(10.99999), 10) &&
std.assertEqual(std.ceil(10), 10) &&
std.assertEqual(std.ceil(10.99999), 11) &&
std.assertEqual(std.sqrt(0), 0) &&
std.assertEqual(std.sqrt(1), 1) &&
std.assertEqual(std.sqrt(9), 3) &&
std.assertEqual(std.sqrt(16), 4) &&
std.assertEqual(std.abs(33), 33) &&
std.assertEqual(std.abs(-33), 33) &&
std.assertEqual(std.abs(0), 0) &&

// Ordinary (non-test) code can define pi as 2*std.acos(0)
local pi = 3.14159265359;

assertClose(std.sin(0.0 * pi), 0) &&
assertClose(std.sin(0.5 * pi), 1) &&
assertClose(std.sin(1.0 * pi), 0) &&
assertClose(std.sin(1.5 * pi), -1) &&
assertClose(std.sin(2.0 * pi), 0) &&
assertClose(std.cos(0.0 * pi), 1) &&
assertClose(std.cos(0.5 * pi), 0) &&
assertClose(std.cos(1.0 * pi), -1) &&
assertClose(std.cos(1.5 * pi), 0) &&
assertClose(std.cos(2.0 * pi), 1) &&
assertClose(std.tan(0), 0) &&
assertClose(std.tan(0.25 * pi), 1) &&
assertClose(std.asin(0), 0) &&
assertClose(std.acos(1), 0) &&
assertClose(std.asin(1), 0.5 * pi) &&
assertClose(std.acos(0), 0.5 * pi) &&
assertClose(std.atan(0), 0) &&
assertClose(std.log(std.exp(5)), 5) &&
assertClose(std.mantissa(1), 0.5) &&
assertClose(std.exponent(1), 1) &&
assertClose(std.mantissa(128), 0.5) &&
assertClose(std.exponent(128), 8) &&

std.assertEqual(std.count([true, false, false, true, true, true, false], true), 4) &&
std.assertEqual(std.count([true, false, false, true, true, true, false], false), 3) &&

std.assertEqual(std.filter(function(x) x % 2 == 0, [1, 2, 3, 4]), [2, 4]) &&
std.assertEqual(std.filter(function(x) false, [1, 2, 3, 4]), []) &&
std.assertEqual(std.filter(function(x) x, []), []) &&

std.assertEqual(std.map(function(x) x * x, []), []) &&
std.assertEqual(std.map(function(x) x * x, [1, 2, 3, 4]), [1, 4, 9, 16]) &&
std.assertEqual(std.map(function(x) x * x, std.filter(function(x) x > 5, std.range(1, 10))), [36, 49, 64, 81, 100]) &&

std.assertEqual(std.mapWithIndex(function(i, x) x * i, []), []) &&
std.assertEqual(std.mapWithIndex(function(i, x) x * i, [1, 2, 3, 4]), [0, 2, 6, 12]) &&
std.assertEqual(std.mapWithIndex(function(i, x) x * i, std.filter(function(x) x > 5, std.range(1, 10))), [0, 7, 16, 27, 40]) &&

std.assertEqual(std.flatMap(function(x) [x, x], [1, 2, 3]), [1, 1, 2, 2, 3, 3]) &&
std.assertEqual(std.flatMap(function(x) if x == 2 then [] else [x], [1, 2, 3]), [1, 3]) &&
std.assertEqual(std.flatMap(function(x) if x == 2 then [] else [x * 3, x * 2], [1, 2, 3]), [3, 2, 9, 6]) &&

std.assertEqual(std.filterMap(function(x) x >= 0, function(x) x * x, [-3, -2, -1, 0, 1, 2, 3]), [0, 1, 4, 9]) &&

std.assertEqual(std.foldl(function(x, y) x + y, [1, 2, 3, 4], 0), 10) &&

# folds with nested arrays are not working
# std.assertEqual(std.foldl(function(x, y) [x, y], [], 'foo'), 'foo') &&
# std.assertEqual(std.foldl(function(x, y) [x, y], [1, 2, 3, 4], []), [[[[[], 1], 2], 3], 4]) &&

# std.assertEqual(std.foldr(function(x, y) [x, y], [], 'bar'), 'bar') &&
# std.assertEqual(std.foldr(function(x, y) [x, y], [1, 2, 3, 4], []), [1, [2, [3, [4, []]]]]) &&

std.assertEqual(std.range(2, 6), [2, 3, 4, 5, 6]) &&
std.assertEqual(std.range(2, 2), [2]) &&
std.assertEqual(std.range(2, 1), []) &&

std.assertEqual(std.flattenArrays([[1, 2], [3, 4], [[5, 6], [7, 8]]]), [ 1, 2, 3, 4, [5, 6], [7, 8]]) &&

true
