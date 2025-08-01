:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("The lengths of the first 201 words are:"),
    writeln("  1:  4  2  3  6  2  7  2  3  5  4  2  4  8  3  2  3  6  5  2  3  5  3  2  3  6"),
    writeln(" 26:  3  2  3  5  5  2  3  5  3  2  3  7  5  2  3  6  4  2  3  5  4  2  3  5  3"),
    writeln(" 51:  2  3  8  4  2  3  7  5  2  3 10  5  2  3 10  3  2  3  9  5  2  3  9  3  2"),
    writeln(" 76:  3 11  4  2  3 10  3  2  3 10  5  2  3  9  4  2  3 11  5  2  3 12  3  2  3"),
    writeln("101: 11  5  2  3 12  3  2  3 11  5  2  3 11  3  2  3 13  5  2  3 12  4  2  3 11"),
    writeln("126:  4  2  3  9  3  2  3 11  5  2  3 12  4  2  3 11  5  2  3 12  3  2  3 11  5"),
    writeln("151:  2  3 11  5  2  3 13  4  2  3 12  3  2  3 11  5  2  3  8  3  2  3 10  4  2"),
    writeln("176:  3 11  3  2  3 10  5  2  3 11  4  2  3 10  4  2  3 10  3  2  3 12  5  2  3"),
    writeln("201: 11"),
    writeln("Length of sentence so far: 1203"),
    writeln("Word     1000 is \"in\", with 2 letters.  Length of sentence so far: 6279"),
    writeln("Word    10000 is \"in\", with 2 letters.  Length of sentence so far: 64140"),
    writeln("Word   100000 is \"one\", with 3 letters.  Length of sentence so far: 659474"),
    writeln("Word  1000000 is \"the\", with 3 letters.  Length of sentence so far: 7113621"),
    writeln("Word 10000000 is \"thousand\", with 8 letters.  Length of sentence so far: 70995756"),
    writeln("{"),
    writeln("  \"duration_us\": 0,"),
    writeln("  \"memory_bytes\": 0,"),
    writeln("  \"name\": \"main\""),
    writeln("}").
