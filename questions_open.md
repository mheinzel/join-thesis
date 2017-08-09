**Open Questions**

Which implementations exist?
    JoCaml (old)
        first implementation by Le Fessant
    JoCaml (new)
        reimplementation
        better compatibility with OCaml
    Polyphonic C#, Cw
    Join Java

How are they implemented?
    JoCaml (old)
    JoCaml (new)
    Polyphonic C#, Cw
    Join Java

Which shortcomings do they have?
    JoCaml (old)
        unmaintained,
        beta version from 2003
    JoCaml (new)
        missing primitives
    Polyphonic C#, Cw
    Join Java

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
