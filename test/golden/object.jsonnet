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

true
