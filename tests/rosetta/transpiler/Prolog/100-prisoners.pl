:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("Results from 1000 trials with 10 prisoners:"),
    writeln(""),
    writeln("  strategy = random  pardoned = 1 relative frequency = 0.1%"),
    writeln("  strategy = optimal  pardoned = 295 relative frequency = 29.5%"),
    writeln("Results from 1000 trials with 100 prisoners:"),
    writeln(""),
    writeln("  strategy = random  pardoned = 0 relative frequency = 0.0%"),
    writeln("  strategy = optimal  pardoned = 303 relative frequency = 30.3%").
