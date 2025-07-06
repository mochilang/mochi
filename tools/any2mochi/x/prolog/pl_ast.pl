:- use_module(library(http/json)).

main :-
    read_stream_to_codes(user_input, Codes),
    open_chars_stream(Codes, S),
    read_terms(S, Terms),
    json_write_dict(current_output, _{clauses:Terms}),
    nl,
    close(S),
    halt.

read_terms(S, [C|Cs]) :-
    read_term(S, Term, [variable_names(Vs), syntax_errors(quiet)]),
    Term \== end_of_file, !,
    clause_ast(Term, Vs, C),
    read_terms(S, Cs).
read_terms(_, []).

clause_ast((Head :- Body), Vs, Dict) :-
    head_info(Head, Vs, Name, Params),
    with_output_to(string(BStr0), write_term(Body, [fullstop(false),spacing(next_argument)])),
    normalize_space(atom(BStr), BStr0),
    Dict = _{name:Name, params:Params, body:BStr}.
clause_ast(Head, Vs, Dict) :-
    head_info(Head, Vs, Name, Params),
    Dict = _{name:Name, params:Params, body:"true"}.

head_info(Term, Vs, Name, Params) :-
    Term =.. [Name|Args],
    maplist(arg_name(Vs), Args, Params).

arg_name(Vs, Var, Name) :- var(Var), !,
    ( member(Var=Name0, Vs) -> Name=Name0 ; Name="_" ).
arg_name(_, Term, Name) :-
    ( atomic(Term) -> Name = Term
    ; with_output_to(string(S), write_term(Term, [quoted(true),numbervars(true),fullstop(false),spacing(next_argument)])),
      normalize_space(atom(Name), S)
    ).

:- initialization(main, main).
