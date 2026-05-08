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
    stream_property(S, position(StartPos)),
    read_term(S, Term, [variable_names(Vs), syntax_errors(quiet)]),
    stream_property(S, position(EndPos)),
    Term \== end_of_file, !,
    clause_ast(Term, Vs, StartPos, EndPos, C),
    read_terms(S, Cs).
read_terms(_, []).

clause_ast((Head :- Body), Vs, Start, End, Dict) :-
    head_info(Head, Vs, Name, Params),
    term_json(Vs, Body, Goal),
    stream_position_data(char_count, Start, StartChar),
    stream_position_data(char_count, End, EndChar),
    Dict = _{name:Name, params:Params, goal:Goal, start:StartChar, end:EndChar}.
clause_ast(Head, Vs, Start, End, Dict) :-
    head_info(Head, Vs, Name, Params),
    stream_position_data(char_count, Start, StartChar),
    stream_position_data(char_count, End, EndChar),
    Dict = _{name:Name, params:Params, goal:true, start:StartChar, end:EndChar}.

head_info(Term, Vs, Name, Params) :-
    Term =.. [Name|Args],
    maplist(term_json(Vs), Args, Params).

var_name(Vs, Var, Name) :-
    ( member(Name0=Var0, Vs), Var == Var0 -> Name = Name0 ; Name = '_' ).

term_json(Vs, Term, JSON) :-
    var(Term), !,
    var_name(Vs, Term, Name),
    JSON = _{var:Name}.
term_json(_, Term, Term) :-
    ( number(Term) ; string(Term) ), !.
term_json(_, Term, Atom) :-
    atom(Term), !,
    Atom = Term.
term_json(Vs, Term, List) :-
    is_list(Term), !,
    maplist(term_json(Vs), Term, List).
term_json(Vs, Term, Dict) :-
    is_dict(Term), !,
    dict_pairs(Term, Tag, Pairs),
    maplist(pair_json(Vs), Pairs, JsonPairs),
    dict_create(Dict, Tag, JsonPairs).
term_json(Vs, Term, Dict) :-
    Term =.. [F|Args],
    maplist(term_json(Vs), Args, JArgs),
    Dict = _{functor:F, args:JArgs}.

pair_json(Vs, K-V, K-J) :-
    term_json(Vs, V, J).

:- initialization(main, main).
