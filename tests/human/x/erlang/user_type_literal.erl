#!/usr/bin/env escript
%% user_type_literal.erl - manual translation of tests/vm/valid/user_type_literal.mochi

main(_) ->
    Book = #{title => "Go", author => #{name => "Bob", age => 42}},
    io:format("~s~n", [maps:get(name, maps:get(author, Book))]).
