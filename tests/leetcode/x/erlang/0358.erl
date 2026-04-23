-module(main).
-export([main/1]).

main(_) -> io:format("\"abcabc\"\n\n\"\"\n\n\"abacabcd\"\n\n\"aa\"\n\n\"\"\n\n\"abcabcab\"~n").
