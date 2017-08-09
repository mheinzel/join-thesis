**Open Questions**

Which implementations exist?
    JoCaml (old)
        compiler extension with locations and go()
        first implementation by Le Fessant
    JoCaml (new)
        compiler extension
        reimplementation
        better compatibility with OCaml (for a price)
    Polyphonic C#
        compiler extension
        developed by Fernet et al. at Microsoft Research
    Cw
        compiler extension
        combination of Polyphonic C# and data storage features
    Joins (concurrency library)
        library for .NET
    Join Java

Which shortcomings do they have?
    JoCaml (old)
        unmaintained,
        beta version from 2003
    JoCaml (new)
        missing primitives (locations, go)
    Polyphonic C#
        not distributed? / no locations
    Cw
        not distributed? / no locations
    Join Java
        not distributed? / no locations

Which requirements do we have for the language we want to use for the implementation?
    moving processes between nodes
    ...

Why is JoCaml implemented as a compiler extension?
    special syntax
    special typechecking
    performance?
    more?

How are exceptions handled in existing implementations?

Is there anything we cannot provide as a library? How could we provide it then?
    compiler extension/plugin
    compiling to the language
    (only if really necessary)
