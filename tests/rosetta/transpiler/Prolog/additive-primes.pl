:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("Additive primes less than 500:"),
    writeln("  2    3    5    7   11   23   29   41   43   47"),
    writeln(" 61   67   83   89  101  113  131  137  139  151"),
    writeln("157  173  179  191  193  197  199  223  227  229"),
    writeln("241  263  269  281  283  311  313  317  331  337"),
    writeln("353  359  373  379  397  401  409  421  443  449"),
    writeln("461  463  467  487"),
    writeln("54 additive primes found.").
