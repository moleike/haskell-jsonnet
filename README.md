# jsonnet-hs

[![Actions Status](https://github.com/moleike/jsonnet-hs/workflows/build/badge.svg)](https://github.com/moleike/jsonnet-hs/actions) [![Join the chat at https://gitter.im/jsonnet-hs/community](https://badges.gitter.im/jsonnet-hs/community.svg)](https://gitter.im/jsonnet-hs/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

A Haskell implementation of the [Jsonnet][jsonnet] configuration language. 
For an introduction to the language itself, see the [tutorial][tutorial] or language [reference][reference].

[jsonnet]: https://jsonnet.org/
[tutorial]: https://jsonnet.org/learning/tutorial.html
[reference]: https://jsonnet.org/ref/language.html

### Build

Using the [stack][stack] build tool:

```bash
git clone github.com/moleike/jsonnet-hs.git
cd jsonnet-hs
stack build
```
[stack]: https://docs.haskellstack.org/en/stable/README


### Language features not yet implemented

- ~~array and object comprehension~~
- ~~array slices~~
- ~~Python-style string formatting~~
- text blocks 
- ~~verbatim strings~~
- ~~object-level locals~~
- object-level asserts
- ~~outermost object reference `$`~~
- ~~keyword parameters~~
- ~~default arguments~~
- top-level arguments
- external variables
- ~~object composition (merging objects)~~
- `super` keyword
- ~~hidden fields~~ ([@CristhianMotoche](https://github.com/CristhianMotoche))
- ~~tailstrict annotation~~

[//]: # "Implementation overview"


## Acknowledgments
I took inspiration from [Expresso][Expresso], [hnix][hnix], [fixplate][fixplate], and numerous other libraries. Thanks to their authors.

[Expresso]: https://github.com/willtim/Expresso
[hnix]: https://github.com/haskell-nix/hnix
[fixplate]: https://hackage.haskell.org/package/fixplate
