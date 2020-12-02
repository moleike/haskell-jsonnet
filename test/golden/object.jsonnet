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

// test sugar for object includes prop
std.assertEqual("x" in { x: 3 }, true) &&
std.assertEqual("x" in { y: 3 }, false) &&

std.assertEqual({ x: 1, a: "x" in self, b: "y" in self }, { x: 1, a: true, b: false }) &&
std.assertEqual({ x:: 1, a: "x" in self, b: "y" in self }, { a: true, b: false }) &&
std.assertEqual({ f: "f" in self }, { f: true }) &&

// Object Local
std.assertEqual({ local foo = 3, local bar(n) = foo + n, x: foo, y: foo + bar(1) }, { x: 3, y: 7 }) &&
//std.assertEqual({ local foo = bar(3)[1], local bar(n) = [foo, n], x: foo, y: [foo] + bar(3) }, { x: 3, y: [3, 3, 3] }) &&

std.assertEqual({ local foo(x) = self.y + x, x: foo(5), y:: 5 }, { x: 10 }) &&

true
