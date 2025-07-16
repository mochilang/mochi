% append-a-record-to-the-end-of-a-text-file.erl - generated from append-a-record-to-the-end-of-a-text-file.mochi

writeTwo() ->
    ["jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash", "jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash"].

appendOneMore(Lines) ->
    Lines ++ ["xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash"].

main() ->
    Lines0 = writeTwo(),
    Lines1 = appendOneMore(Lines0),
    (case ((length(Lines1) >= 3) andalso (lists:nth((2)+1, Lines1) == "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash")) of true -> io:format("~p~n", ["append okay"]); _ -> io:format("~p~n", ["it didn't work"]) end).

main(_) ->
    main().
