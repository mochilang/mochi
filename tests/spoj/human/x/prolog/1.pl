% https://www.spoj.com/problems/TEST/
:- use_module(library(readutil)).
:- initialization(main).

main :- solve, halt.

solve :-
    read_line_to_codes(user_input, Codes),
    ( Codes == end_of_file ->
        true
    ; number_codes(N, Codes),
      ( N =:= 42 ->
          true
        ; format('~d~n', [N]),
          solve
      )
    ).
