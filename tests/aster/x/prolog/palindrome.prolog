:- initialization(main).
:- style_check(-singleton).
is_palindrome(L) :-
    reverse(L, L).
main :-
    is_palindrome([1, 2, 3, 2, 1]) -> writeln(true); writeln(false).
