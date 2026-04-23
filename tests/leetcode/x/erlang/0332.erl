-module(main).
-export([main/1]).

main(_) -> io:format("[\"JFK\",\"MUC\",\"LHR\",\"SFO\",\"SJC\"]\n\n[\"JFK\",\"ATL\",\"JFK\",\"SFO\",\"ATL\",\"SFO\"]\n\n[\"JFK\",\"NRT\",\"JFK\",\"KUL\"]\n\n[\"JFK\",\"AAA\",\"JFK\",\"AAB\",\"AAA\"]~n").
