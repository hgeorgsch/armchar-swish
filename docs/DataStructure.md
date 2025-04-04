---
title: Data Structure
tags:
    - armchar/swish
---


+ `ArM.STM` - Software Transactional Memory (STM) 
    + stores three graphs
        1. `schemaGraph`
        2. `resourceGraph`
        3. `graph` (merging character data with the other two graphs)
+ CharacterMap contains all the charactersheets for a given character
    + generated by `ArM.Character.getALlCS`
+ `ArM.STM.lookup` 
    + takes character, year, and season as arguments
    + retrieves the graphs from the STM
    + generates the CharacterMap
    + returns the right Character Sheet from the Map.

## The STM store

The three graphs stored in STM are prepared as follows.
The processing is done in the `Load` module, and the three graphs
returned are added to a `MapState` object.

The arrow labels are function names.
The «raw» data objects correspond to files.

```
      prepareCharGraph                              prepareInitialCharacter
raw char ------------> graph1 ------> merge ---> graph2 --------------
                                        ^                            |
           prepareSchema                |                            |
raw schema ------------> schemaGraph -->|                            |
                                        |                            |
            prepareResources            v  applyRDFS               merge
raw resources ---------> res1 ------->merge -------> resourceGraph ->|
                                                                     |
                                                       prepareGraph  |
                                               graph <----------------
```

**Error** It is `res`` and not `resourceGraph` which is merged into 
the character graph.

### Notes

+ `prepareCharGraph` makes only a few inferences to simplify future queries
+ `prepareSchema` does subclass inference and similar rules
+ `prepareInitialCharacter` makes the CharacterSheet from the Character
    - character inherit data from covenant or saga
+ `prepareGraph` copies data from the resource graph to make generic
  descriptions available directly in the character sheet
    - trait inherit description from class

## Derivation of the CharacterSheet

+ Fungtion getting data from the graph (from STM)
    -  returning lists of advancements for a given character
        - `getPregameAdvancements`
        - `getIngameAdvancements`
    - returning character sheets
        - `getInitialCS` (before pregame advancements)
        - `getGameStartCharacter` (at game start)
    - `advanceCharacter` takes an advancement and a sheet and makes advances
      to the next sheet
    - `getAllCS` makes a list of all in-game sheets using the above functions
- `ArM.STM.lookup` makes a `CharacterMap` from the `graph`
    - make all the charactersheets with `getAllCS`
    - each sheet is converted to a graph
    - apply `prepareRecord` on each sheet

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

