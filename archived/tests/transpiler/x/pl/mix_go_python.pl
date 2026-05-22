:- initialization(main).
:- style_check(-singleton).

main :-
    RawName = "   alice  ",
    Radius = 3,
    Strings.ToUpper(Strings.TrimSpace("   alice  "), Name),
    Area is 3.141592653589793 * Math.pow(3, 2),
write("Hello"), write(' '), writeln(Name + "!"),
write("The area of a circle with radius"), write(' '), write(3), write(' '), write("is"), write(' '), writeln(Area).
