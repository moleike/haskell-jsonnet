# haskell-jsonnet

[![Actions Status](https://github.com/moleike/jsonnet-hs/workflows/build/badge.svg)](https://github.com/moleike/jsonnet-hs/actions) [![Join the chat at https://gitter.im/jsonnet-hs/community](https://badges.gitter.im/jsonnet-hs/community.svg)](https://gitter.im/jsonnet-hs/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

This is an (unofficial) Haskell implementation of the [Jsonnet][jsonnet] configuration language.
For an introduction to the language itself, see the [tutorial][tutorial] or language [reference][reference].
We are using the same test suite used in the C++ and Go implementations (which is very comprehensive).

[jsonnet]: https://jsonnet.org/
[tutorial]: https://jsonnet.org/learning/tutorial.html
[reference]: https://jsonnet.org/ref/language.html

## Build

Using the [stack][stack] build tool:

```bash
git clone github.com/moleike/jsonnet-hs.git
cd jsonnet-hs
stack build
```
[stack]: https://docs.haskellstack.org/en/stable/README

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
- [X] object composition (`self` and merging objects)
- [X] outermost object reference `$`                                           
- [ ] `super` keyword
- [ ] overriding deeply nested fields (`+:` field syntax)                      
- [X] hidden fields ([@CristhianMotoche](https://github.com/CristhianMotoche)) 
- [X] tailstrict annotation                                                    

[//]: # "Implementation overview"


## Acknowledgments
I took inspiration from [Expresso][Expresso], [hnix][hnix], [fixplate][fixplate], and numerous other libraries. Thanks to their authors.

[Expresso]: https://github.com/willtim/Expresso
[hnix]: https://github.com/haskell-nix/hnix
[fixplate]: https://hackage.haskell.org/package/fixplate
