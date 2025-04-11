# haskell-jsonnet

[![Actions Status](https://github.com/moleike/haskell-jsonnet/workflows/build/badge.svg)](https://github.com/moleike/haskell-jsonnet/actions)
[![Hackage](https://img.shields.io/hackage/v/jsonnet.svg?logo=haskell)](https://hackage.haskell.org/package/jsonnet)

A full-fledged Haskell implementation of the [Jsonnet][jsonnet] spec.
For an introduction to the language itself, see the [tutorial][tutorial] or language [reference][reference].
We are using the same test suite used in the offical [C++][cpp-jsonnet] and [Go][go-jsonnet] implementation (which is fairly comprehensive).

## Progress

Here is the implementation status of the main language features:

- [X] array and object comprehension
- [X] array slices
- [X] Python-style string formatting
- [X] text blocks
- [X] verbatim strings
- [X] object-level locals
- [ ] object-level asserts (https://github.com/moleike/haskell-jsonnet/issues/39)
- [X] keyword parameters
- [X] default arguments
- [ ] top-level arguments (https://github.com/moleike/haskell-jsonnet/issues/24)
- [X] external variables (by Berk Özkütük)
- [X] hidden fields (by Cristhian Motoche)
- [X] tailstrict annotation
- [X] outermost object reference `$`
- [X] mixin inheritence and open recursion (operator `+` with `self` and `super`)
- [X] field composition (operator `+:`)
- [X] multiple file output (by Berk Özkütük)
- [X] verbatim imports (by Berk Özkütük)

## Build

Using the [stack][stack] build tool:

```console
% git clone github.com/moleike/haskell-jsonnet.git
% cd haskell-jsonnet
% stack build
```

## Install

```console
% stack install # to install
```

## Usage

```console
% hs-jsonnet --help
Usage: hs-jsonnet [-v|--version] [-e|--exec] [<filename>]
                  [(-o|--output-file <filename>) | (-m|--multi <dir>)]
                  [--compact] [-S|--string] [-V|--ext-str VAR=VALUE]
                  [--ext-str-file FILE] [--ext-code VAR=EXPR]
                  [--ext-code-file FILE]

Available options:
  -v,--version             Print version of the program
  -h,--help                Show this help text
  -e,--exec                Treat filename as code
  <filename>               Jsonnet source file or stdin
  -o,--output-file <filename>
                           Write to the output file rather than stdout
  -m,--multi <dir>         Write multiple files to the directory, list files on
                           stdout
  --compact                Produce a compact JSON output
  -S,--string              Expect a string, manifest as plain text
  -V,--ext-str VAR=VALUE   External string variable
  --ext-str-file FILE      External string variable as file
  --ext-code VAR=EXPR      External code variable
  --ext-code-file FILE     External code variable as file
```

## Output formats

By default Jsonnet programs evaluate to a JSON document, serialized using `aeson`. 

The `std` library provides several methods to output other formats, e.g. 
to generate a Yaml stream instead:

``` console
% hs-jsonnet -S -e "std.manifestYamlStream(['a', 1, []])"
---
"a"
---
1
---
[]
...
```

Note the we need to use the option `-S` to output a verbatim string, instead of default JSON.

Similarly, to output prettified JSON: 

``` console
% cat pretty.jsonnet 
std.manifestJsonEx(
{
    x: [1, 2, 3, true, false, null,
        "string\nstring"],
    y: { a: 1, b: 2, c: [1, 2] },
}, "    ")

% hs-jsonnet -S pretty.jsonnet
{
    "x": [
        1,
        2,
        3,
        true,
        false,
        null,
        "string\nstring"
    ],
    "y": {
        "a": 1,
        "b": 2,
        "c": [
            1,
            2
        ]
    }
}
```

See the Standard library [documentation][stdlib] for more details.


[//]: # "Implementation overview"

## Benchmarks

Preliminary [results][benchmark-gist] using the benchmarks [here][cpp-benchmarks] 
for comparison.

## Contributing

See [CONTRIBUTING.md][contributing].

## Acknowledgments

I took inspiration from [Expresso][Expresso], [hnix][hnix], [fixplate][fixplate], [Disco][disco], 
and numerous other libraries. Thanks to their authors.

## License

See [LICENSE][license].

Copyright © 2020–present Alexandre Moreno

[jsonnet]: https://jsonnet.org/
[tutorial]: https://jsonnet.org/learning/tutorial.html
[reference]: https://jsonnet.org/ref/language.html
[stack]: https://docs.haskellstack.org/en/stable/README
[Expresso]: https://github.com/willtim/Expresso
[hnix]: https://github.com/haskell-nix/hnix
[fixplate]: https://hackage.haskell.org/package/fixplate
[disco]: https://github.com/disco-lang/disco
[contributing]: https://github.com/moleike/haskell-jsonnet/blob/master/CONTRIBUTING.md
[license]: https://github.com/moleike/haskell-jsonnet/blob/master/LICENSE
[cpp-jsonnet]: https://github.com/google/jsonnet
[cpp-benchmarks]: https://github.com/google/jsonnet/tree/master/benchmarks
[go-jsonnet]: https://github.com/google/go-jsonnet
[benchmark-gist]: https://gist.github.com/moleike/17d5de15be06b05ddad317fe1fcf95a5
[stdlib]: https://jsonnet.org/ref/stdlib.html
