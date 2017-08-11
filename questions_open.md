**Open Questions**

Which implementations exist?
    JoCaml (old)
        compiler extension by Le Fessant
        has locations and go()
        unmaintained since 2003
    JoCaml (new)
        compiler extension
        reimplementation
        better compatibility with OCaml (for a price)
        not distributed? / no locations
    Polyphonic C#
        compiler extension
        developed by Fernet et al. at Microsoft Research
        not distributed? / no locations
    Cw
        compiler extension
        combination of Polyphonic C# and data storage features
        not distributed? / no locations
    Join Java
        compiler extension
        not distributed? / no locations
    Joins (concurrency library)
        library for .NET
    Scala Joins
        library using extensible pattern matching
        integrated into actors
        TODO: how can you specify distribution?
    Chymyst
        Scala library
        not distributed? / no locations
    Funnel
        "functional nets" language implementation by Odersky
        join as extension of lambda calculus as new programming paradigm
        extends join to objects
        Funnel0 dynamically, Funnel1 statically typed
        not distributed? / no locations
    JErlang
    SCHOOL
        new, minimal language
    C++ Boost join

Which shortcomings do they have?
    JoCaml (old)
        unmaintained,
        beta version from 2003
    JoCaml (new)
    Polyphonic C#
    Cw
        not distributed? / no locations
    Join Java
        not distributed? / no locations

!!!
is there a better way to model distribution than through name servers?
    TODO:
        analyse old JoCaml
            how does `location` primitive work?
        read about new JoCaml implementation
            why nameservers?
        think about using `location` and `go` for specifying distribution


Which requirements do we have for the language we want to use for the implementation?
    moving processes between nodes
    ...

Why is JoCaml implemented as a compiler extension?
    special syntax
    special typechecking
    performance?
    more?
    compiling pattern matches

How are exceptions handled in existing implementations?

Is there anything we cannot provide as a library? How could we provide it then?
    compiler extension/plugin
    compiling to the language
    (only if really necessary)
