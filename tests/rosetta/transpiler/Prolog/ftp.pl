:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("Connected to localhost:21"),
    writeln("Logged in as anonymous"),
    writeln("pub"),
    writeln("somefile.bin 35"),
    writeln("readme.txt 15"),
    writeln("Wrote 35 bytes to somefile.bin").
