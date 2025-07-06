:- use_module(library(http/json)).

main([File]) :-
    setup_call_cleanup(open(File, read, In),
        read_terms(In, Terms),
        close(In)),
    json_write_dict(current_output, Terms),
    nl.
main(_) :-
    writeln('Usage: pl2ast File'),
    halt(1).

read_terms(In, Terms) :-
    read_term(In, Term, [variable_names(Vars), syntax_errors(dec10)]),
    ( Term == end_of_file ->
        Terms = []
    ;
        term_to_ast(Term, Vars, AST),
        read_terms(In, Rest),
        Terms = [AST|Rest]
    ).

term_to_ast((Head :- Body), Vars, AST) :- !,
    head_info(Head, Vars, Name, Params),
    term_string(Body, BodyStr),
    AST = _{name:Name, params:Params, body:BodyStr}.
term_to_ast(Head, Vars, AST) :-
    head_info(Head, Vars, Name, Params),
    AST = _{name:Name, params:Params, body:"true"}.

head_info(Head, Vars, Name, Params) :-
    Head =.. [Name|Args],
    maplist(arg_name(Vars), Args, Params).

arg_name(Vars, Var, Name) :-
    ( var(Var) ->
        ( member(Name=Var0, Vars), Var == Var0 -> true ; term_string(Var, Name) )
    ; term_string(Var, Name)
    ).
:- initialization(main, main).
