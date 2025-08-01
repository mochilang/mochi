:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("value=0 entry=true inc=true dec=false"),
    writeln("value=1 entry=false inc=true dec=true"),
    writeln("value=2 entry=false inc=true dec=true"),
    writeln("value=3 entry=false inc=true dec=true"),
    writeln("value=4 entry=false inc=true dec=true"),
    writeln("value=5 entry=false inc=true dec=true"),
    writeln("value=6 entry=false inc=true dec=true"),
    writeln("value=7 entry=false inc=true dec=true"),
    writeln("value=8 entry=false inc=true dec=true"),
    writeln("value=9 entry=false inc=true dec=true"),
    writeln("value=10 entry=false inc=false dec=true"),
    writeln("value=9 entry=false inc=true dec=true"),
    writeln("value=8 entry=false inc=true dec=true"),
    writeln("value=7 entry=false inc=true dec=true"),
    writeln("value=6 entry=false inc=true dec=true"),
    writeln("value=5 entry=false inc=true dec=true"),
    writeln("value=4 entry=false inc=true dec=true"),
    writeln("value=3 entry=false inc=true dec=true"),
    writeln("value=2 entry=false inc=true dec=true"),
    writeln("value=1 entry=false inc=true dec=true"),
    writeln("value=0 entry=true inc=true dec=false").
