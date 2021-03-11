# haskell-jsonnet

[![Actions Status](https://github.com/moleike/jsonnet-hs/workflows/build/badge.svg)](https://github.com/moleike/jsonnet-hs/actions) [![Join the chat at https://gitter.im/jsonnet-hs/community](https://badges.gitter.im/jsonnet-hs/community.svg)](https://gitter.im/jsonnet-hs/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

This is an (unofficial) Haskell implementation of the [Jsonnet][jsonnet] configuration language.
For an introduction to the language itself, see the [tutorial][tutorial] or language [reference][reference].
We are using the same test suite used in the C++ and Go implementations (which is very comprehensive).


## Build

Using the [stack][stack] build tool:

```bash
git clone github.com/moleike/jsonnet-hs.git
cd jsonnet-hs
stack build
```

## Progress

Here is the implementation status of the main language features:

- [X] array and object comprehension
- [X] array slices
- [X] Python-style string formatting                                           
- [X] text blocks                                                              
- [X] verbatim strings                                                         
- [X] object-level locals                                                      
- [ ] object-level asserts                                                     
- [X] keyword parameters                                                       
- [X] default arguments                                                        
- [ ] top-level arguments                                                      
- [ ] external variables                                                       
- [X] hidden fields ([@CristhianMotoche](https://github.com/CristhianMotoche)) 
- [X] tailstrict annotation                                                    

OO features are implemented but need some more work:
- [X] `self` keyword
- [X] `super` keyword
- [X] outermost object reference `$`                                           
- [X] object composition (merging objects)
- [X] field composition (`+:` field syntax)                      

[//]: # "Implementation overview"



## Contributing

See [CONTRIBUTING.md][contributing].

## Acknowledgments

I took inspiration from [Expresso][Expresso], [hnix][hnix], [fixplate][fixplate], and numerous other libraries. Thanks to their authors.

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
[contributing]: https://github.com/moleike/haskell-jsonnet/blob/master/CONTRIBUTING.md
[license]: https://github.com/moleike/haskell-jsonnet/blob/master/LICENSE

