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

local fibonacci(n) =
  if n <= 1 then
    1
  else
    fibonacci(n - 1) + fibonacci(n - 2);

std.assertEqual(fibonacci(15), 987) &&

# mutual recursion
local is_even = function(n) if n == 0 then true else is_odd(n - 1),
      is_odd = function(n) if n == 1 then true else is_even(n - 1);

std.assertEqual(is_even(42), true) &&
std.assertEqual(is_odd(41), true) &&

# lazy version of fix
local fix(f) = local x = f(x); x;

local factorial(n) =
    local step(rec) =
        function(n) if n == 0 then 1 else n * rec(n-1);
    fix(step)(n);

std.assertEqual(factorial(5), 120) &&

# strict version of fix
local fix_(f) = f(fix_(f));

local factorial_(n) =
    local step(rec) =
        function(n) if n == 0 then 1 else n * rec(n-1);
    fix_(step)(n);

std.assertEqual(factorial_(5), 120) &&

true
