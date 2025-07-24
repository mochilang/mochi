:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("The first 100 arithmetic numbers are:"),
    writeln("  1   3   5   6   7  11  13  14  15  17"),
    writeln(" 19  20  21  22  23  27  29  30  31  33"),
    writeln(" 35  37  38  39  41  42  43  44  45  46"),
    writeln(" 47  49  51  53  54  55  56  57  59  60"),
    writeln(" 61  62  65  66  67  68  69  70  71  73"),
    writeln(" 77  78  79  83  85  86  87  89  91  92"),
    writeln(" 93  94  95  96  97  99 101 102 103 105"),
    writeln("107 109 110 111 113 114 115 116 118 119"),
    writeln("123 125 126 127 129 131 132 133 134 135"),
    writeln("137 138 139 140 141 142 143 145 147 149"),
    writeln(""),
    writeln("The 1,000th arithmetic number is: 1,361"),
    writeln("The count of such numbers <= 1,361 which are composite is 782."),
    writeln(""),
    writeln("The 10,000th arithmetic number is: 12,953"),
    writeln("The count of such numbers <= 12,953 which are composite is 8,458."),
    writeln(""),
    writeln("The 100,000th arithmetic number is: 125,587"),
    writeln("The count of such numbers <= 125,587 which are composite is 88,219."),
    writeln(""),
    writeln("The 1,000,000th arithmetic number is: 1,228,663"),
    writeln("The count of such numbers <= 1,228,663 which are composite is 905,043.").
