# jsonnet-haskell

[![Actions Status](https://github.com/moleike/jsonnet-haskell/workflows/build/badge.svg)](https://github.com/moleike/jsonnet-haskell/actions) [![Join the chat at https://gitter.im/jsonnet-haskell/community](https://badges.gitter.im/jsonnet-haskell/community.svg)](https://gitter.im/jsonnet-haskell/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

A Haskell implementation of the [Jsonnet][jsonnet] configuration language. 
For an introduction to the language itself, see the [tutorial][tutorial] or language [reference][reference].

[jsonnet]: https://jsonnet.org/
[tutorial]: https://jsonnet.org/learning/tutorial.html
[reference]: https://jsonnet.org/ref/language.html

## Language features not yet implemented

- [ ] array and object comprehension
- [ ] array slices
- [ ] Python-style string formatting
- [ ] text blocks 
- [ ] verbatim strings
- [ ] object-level locals and asserts
- [ ] keyword parameters 
- [ ] default arguments
- [ ] top-level arguments
- [ ] external variables
- [ ] object composition (merging objects)
- [ ] hidden fields
- [ ] `super` keyword
- [ ] tailstrict annotation

[//]: # "Implementation overview"


## Acknowledgments
I took inspiration from [Expresso][Expresso], [hnix][hnix], and numerous other libraries. Thanks to their authors.

[Expresso]: https://github.com/willtim/Expresso
[hnix]: https://github.com/haskell-nix/hnix
