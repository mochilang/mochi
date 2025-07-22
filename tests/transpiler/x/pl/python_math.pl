:- initialization(main).
:- style_check(-singleton).

main :-
    R = 3,
    Area is 3.141592653589793 * Math.pow(3, 2),
    Math.sqrt(49, Root),
    Math.sin(3.141592653589793 / 4, Sin45),
    Math.log(2.718281828459045, Log_e),
write("Circle area with r ="), write(' '), write(3), write(' '), write("=>"), write(' '), writeln(Area),
write("Square root of 49:"), write(' '), writeln(Root),
write("sin(π/4):"), write(' '), writeln(Sin45),
write("log(e):"), write(' '), writeln(Log_e).
