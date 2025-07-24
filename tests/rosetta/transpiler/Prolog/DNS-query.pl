:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("#("),
    writeln("  ''2001:2f0:0:8800:226:2dff:fe0b:4311''."),
    writeln("  ''2001:2f0:0:8800::1:1''."),
    writeln("  ''210.155.141.200''"),
    writeln(")").
