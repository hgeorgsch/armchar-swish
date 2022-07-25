# armchar-swish

ArM character server implementation using Haskell and Swish.

The program, `armchar-swish`, is currently only for testing.
It has no real features.

## Design notes

+ [Update](Design/Update)
+ [Ontology](Ontology/README.md)
+ [Tests](Tests.md) showing current features of the web API

## Testing

The easiest way to run the program is via cabal:
```
cabal run
```

It starts a web server on port 3000.  
The most convenient way to test it is probably the
[HTTPie](https://httpie.io/) tool.
```
% http get :3000/
HTTP/1.1 200 OK
Content-Type: text/plain; charset=utf-8
Date: Thu, 21 Apr 2022 20:51:46 GMT
Server: Warp/3.3.20
Transfer-Encoding: chunked

Test a get call - available paths for get:
  /    (this page)
  /graph
  /initial
```

Testing PUT calls is a bit tricky, as `http` does not read from the terminal,
but the following does work (as an example):
```
cat | http put :3000/adv
{
        "advancementcontents": {
            "arm:atSeason": "Autumn",
            "arm:awardsXP": 16,
            "arm:hasAdvancementDescription": "Studies Mentem L16 Q13 +3",
            "arm:hasAdvancementIndex": 100,
            "arm:hasAdvancementType": {
                "prefixedid": "arm:Reading"
            },
            "arm:hasAdvancementTypeString": "Reading",
            "arm:inYear": 1217
        },
        "advancementid": "<https://hg.schaathun.net/armchar/character/cieran#autumn1217adv>",
        "advancementtraits": [
            {
                "arm:addedXP": 16,
                "arm:hasLabel": "Mentem",
                "arm:isSpecialTrait": {
                    "prefixedid": "arm:AccelleratedTrait"
                },
                "arm:prefixedid": "_:73",
                "arm:traitClass": {
                    "prefixedid": "armr:mentem"
                }
            }
        ]
    }
```
Obviously, if you have a JSON test file, you can redirect from file instead.

## Problems

There are several limitations in Swish compared to Jena
as far as I can tell:

+ No prebuilt OWL and RDFS reasoners.  
    - However, reasoning is expensive and only a fraction of the OWL
      and RDFS inferences are actually useful for us.
+ No ready to use function to apply rulesets.
    - However, such generic functions could be expensive.
    - A recursive function 'fwdApplyListR' has been implemented
      to solve this problem and it seems to work well.
+ Rules cannot easily be defined in a separate file in a separate
  rules language.  The focus of Swish has been the script language.
    - Hence, the 'ArM.Rules' module is clunky
+ No JSON-LD support
    - However, not using JSON-LD may make the client a lot easier
      to implement
+ No noValue clause
    - However, the noValue clause make the reasoner expensive.
    - Coding the inference without noValue is more efficient.


## Graph Processing

The following diagram shows the preparation of the graph in 
the `Load` module.
The arrow labels are function names.
The «raw» data objects correspond to files.
Many of the transformations are not fully implemented yet, but
the main principles have been demonstrated.

```
               prepareCS
raw character  --------> preliminary graph --
                                            |
           prepareSchema                    |
raw schema ------------> schema graph ------| merge
                                            |
                    prepareInitialCharacter |
                                            v
raw resources                            Character Graph
    |                  merge                |
    -----------------------------------------
    prepareResources     |
                         | prepareGraph
                         v
          cgraph (initial graph from ArM.Load.getGraph)
                         |
                         | advanceCharacter
                         v
                 character sheet per season
  (separate graph - not merged in with the schema and resources)
                         |
                         | schemaReasoner
                         v
                 character sheet for client queries
```

+ `prepareCS` makes only a few inferences to simplify future queries
+ `prepareSchema` does subclass inference and similar rules
+ `prepareInitialCharacter` makes the CharacterSheet from the Character
    - character inherit data from covenant or saga
+ `prepareGraph` copies data from the resource graph to make generic
  descriptions available directly in the character sheet
    - trait inherit description from class
+ `advanceCharacter` does not currently use `RDFGraph`.
    - The character sheets are generated an internal Haskell type
    - We consider putting the character sheet back into the graph. 
    - Doing the reasoning on the graph is too costly in this case
+ `schemaReasoner` closes the graph under RDFS subClassOf and
  subPropertyOf relations.  It also deduces subproperties of hasTrait,
  by using the class of the object.  **not tested**

### Graphs and Data Types in use

1.  Character as Loaded from File
    - Base Character
    - Initial Character Sheet
    - Advancement per Season
2.  Supporting Ontologies (separate files)
    -  Resources
    -  ArM Schema (mainly for use with OWL/RDFS reasoners)
4.  `cgraph` : Derived Character Sheet with implied traits
    - this is created by `prepareGraph` and then loaded
      into a CharacterSheet type
5.  Internal Data Types are loaded from `cgraph` 
    - CharacterSheet
    - Advancement
    - Trait (only as subsidiaries to CharacterSheet and Advancement)
6.  `CharacterRecord` Derived Character Sheet per Season
    - map `CharacterSheet -> Advancement -> CharacterSheet`
    - this is easily applied with `foldl`
    - The resulting CharacterSheet is converted to RDFGraph
      and wrapped as `CharacterRecord`
    - Stored in a `CharacterMap`
    - schema and resources are not included in this graph
7.  Complete CharacterSheet for Display
    - Before display, a reasoner is needed to add properties
      used by the query.
    - An RDF reasoner uses type relations to sort different
      types of traits to make display processing simple

## TODO

### Project 1.  Managing a single character

1. Discuss Web API 
2. Test and review
3. Web Server - put advancement resource
4. Generate documentation
5. Spell String Rules
6. Make LaTeX
    1.  Pull metadata
    2.  Pull Characteristics

### Project 2.  Managing a covenant/saga

1.  Library resources
    - link books to advancements
    - check for conflicts
2.  Joint advancement log (view)
3.  Other shared resources
4.  Finances
    - low maintenance covenfolk



## Overview

+ Main  - the current version includes a lot of debug displays,
  in addition to the main feature from ArM.WebServices
+ ArM.WebServices OK defines the scotty action which is used in Main.
+ ArM.Load (OK) loads the graph and applies initial reasoners from 
  ArM.Rules and ArM.Rules.*
    - this results in three graphs (g,schema,res), where g contains the 
      character data
+ ArM.Character 
    - This contains the data types and functions to 
        1. load character data from the graph g (above)
	2. advance the characters and produce new graph for each
	   advancement period, containing the character sheet at that time.
    - internal implementation is in ArM.Character.*
+ ArM.CharacterMap is the data store for character sheets.
    - `Main` uses `ArM.Character.getAllCS` to create all the character sheets.
    - These character sheets are stored using `ArM.CharacterMap.insertListS`
    - Before storage, a reasoner is applied to the character sheet
      to add inferred information to make queries easier. 
+ ArM.CharacterQuery OK  This is a very simple set of functions
  to get data a graph from the data store (above).
+ ArM.JSON This module contains all the code to create JSON
  from the objects defined in other modules.
    - **TODO** implement `FromJSON`
+ Auxiliary modules
    + ArM.BlankNode  OK  This is a simple monad to produce unique
      blank nodes based on a serial number.  This is used in ArM.Character
    + ArM.KeyPair  OK  This module collects semi-flexible functions
      to query graphs.  These are used in ArM.Character
+ ArM.Resources This names resources used in the project.
  This is not used consistently
+ ArM.Rules  The reasoner(s) is a bit of a mess
    + ArM.Rules.Aux
    + ArM.Rules.FullGraph
    + ArM.Rules.RDFS
    + ArM.Rules.Resource
    + ArM.Rules.Schema

## References

+ [STM](https://gilmi.me/blog/post/2020/12/05/scotty-bulletin-board#logging-sessions-cookies-authentication-etc.)  tutorial
