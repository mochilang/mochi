#!/usr/bin/env escript
% user_type_literal.erl - generated from user_type_literal.mochi

main(_) ->
    Book = #{"__name" => "Book", title => "Go", author => #{"__name" => "Person", name => "Bob", age => 42}},
    io:format("~p~n", [maps:get(name, maps:get(author, Book))]).
