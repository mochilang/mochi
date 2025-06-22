//go:build ignore
// +build ignore

next:
    MOV R0, "for"
    DROP R0
    MOV R1, "if"
    DROP R1
    MOV R2, "src"
    CALL len, R3, R2
    DROP R3
    MOV R4, "tk"
    DROP R4
    MOV R5, "lit"
    DROP R5
    RET R0
    MOV R6, "ch"
    DROP R6
    MOV R7, "pos"
    DROP R7
    MOV R8, "switch"
    DROP R8
    DROP R0
    DROP R0
    DROP R0
    DROP R0
    MOV R9, "continue"
    DROP R9
    DROP R0
    MOV R10, "LPAREN"
    DROP R10
    MOV R11, "("
    DROP R11
    MOV R12, "case"
    DROP R12
    MOV R13, "tk"
    DROP R13
    MOV R14, "lit"
    DROP R14
    MOV R15, "case"
    RET R15
    DROP R0
    MOV R16, "LBRACE"
    DROP R16
    MOV R17, "{"
    DROP R17
    MOV R18, "case"
    DROP R18
    MOV R19, "tk"
    DROP R19
    MOV R20, "lit"
    DROP R20
    MOV R21, "case"
    RET R21
    MOV R22, "tk"
    DROP R22
    MOV R23, "lit"
    DROP R23
    MOV R24, "case"
    RET R24
    DROP R0
    MOV R25, "COMMA"
    DROP R25
    MOV R26, ","
    DROP R26
    MOV R27, "case"
    DROP R27
    MOV R28, "pos"
    DROP R28
    DROP R0
    DROP R0
    MOV R29, "pos"
    DROP R29
    MOV R30, "for"
    DROP R30
    MOV R31, "src"
    CALL len, R32, R31
    DROP R32
    MOV R33, "pos"
    DROP R33
    DROP R0
    DROP R0
    DROP R0
    MOV R34, "':\n\t\t\tstart := pos\n\t\t\tfor pos < len(src) && src[pos] != '"
    DROP R34
    MOV R35, "pos"
    DROP R35
    MOV R36, "lit"
    DROP R36
    DROP R0
    MOV R37, "start"
    DROP R37
    DROP R0
    MOV R38, "pos"
    DROP R38
    DROP R0
    DROP R0
    MOV R39, "pos"
    DROP R39
    MOV R40, "tk"
    DROP R40
    MOV R41, "default"
    RET R41
    MOV R42, "ch"
    CALL if, R43, R42
    DROP R43
    MOV R44, "ch"
    DROP R44
    DROP R0
    MOV R45, "ch"
    DROP R45
    MOV R46, "ch"
    DROP R46
    DROP R0
    MOV R47, "_"
    DROP R47
    MOV R48, "start"
    DROP R48
    MOV R49, "1"
    DROP R49
    MOV R50, "pos"
    DROP R50
    DROP R0
    DROP R0
    MOV R51, "c"
    DROP R51
    MOV R52, "pos"
    DROP R52
    DROP R0
    MOV R53, "a"
    DROP R53
    MOV R54, "z"
    DROP R54
    DROP R0
    MOV R55, "A"
    DROP R55
    MOV R56, "Z"
    DROP R56
    DROP R0
    MOV R57, "0"
    DROP R57
    MOV R58, "9"
    DROP R58
    MOV R59, "c"
    DROP R59
    DROP R0
    DROP R0
    DROP R0
    DROP R0
    MOV R60, "lit"
    DROP R60
    DROP R0
    MOV R61, "start"
    DROP R61
    DROP R0
    MOV R62, "lit"
    DROP R62
    MOV R63, "case"
    DROP R63
    MOV R64, "tk"
    DROP R64
    MOV R65, "case"
    DROP R65
    MOV R66, "tk"
    DROP R66
    MOV R67, "case"
    DROP R67
    MOV R68, "tk"
    DROP R68
    MOV R69, "default"
    DROP R69
    MOV R70, "IDENT"
    DROP R70
    RET R0
    MOV R71, "if"
    DROP R71
    MOV R72, "0"
    DROP R72
    MOV R73, "9"
    DROP R73
    MOV R74, "start"
    DROP R74
    MOV R75, "1"
    DROP R75
    MOV R76, "pos"
    DROP R76
    DROP R0
    DROP R0
    MOV R77, "pos"
    DROP R77
    MOV R78, "src"
    DROP R78
    MOV R79, "9"
    DROP R79
    MOV R80, "pos"
    DROP R80
    MOV R81, "lit"
    DROP R81
    DROP R0
    MOV R82, "start"
    DROP R82
    DROP R0
    MOV R83, "INT"
    DROP R83
    DROP R0
    DROP R0
stmt:
    MOV R0, "if"
    DROP R0
    MOV R1, "RETURN"
    DROP R1
    CALL next, R2
    DROP R2
    CALL expr, R3
    DROP R3
    MOV R4, "cur"
    MOV R5, "instr"
    CALL append, R6, R4
    DROP R6
    MOV R7, "op"
    DROP R7
    DROP R0
    MOV R8, "r"
    DROP R8
    DROP R0
    MOV R9, "tk"
    DROP R9
    DROP R0
    DROP R0
    DROP R0
    DROP R0
    CALL expr, R10
    DROP R10
    MOV R11, "cur"
    MOV R12, "instr"
    CALL append, R13, R11
    DROP R13
    MOV R14, "op"
    DROP R14
    DROP R0
    MOV R15, "r"
    DROP R15
    DROP R0
    MOV R16, "tk"
    DROP R16
    DROP R0
    DROP R0
    DROP R0
    MOV R17, "tk"
    DROP R17
    MOV R18, "tk"
    DROP R18
    DROP R0
    DROP R0
    DROP R0
main:
    MOV R0, "show"
    DROP R0
    MOV R1, "s"
    MOV R2, "false"
    MOV R3, "show instructions"
    CALL Bool, R4, R1
    DROP R4
    CALL Parse, R5
    DROP R5
    MOV R6, "flag"
    DROP R6
    DROP R0
    MOV R7, "0"
    DROP R7
    MOV R8, "fmt"
    DROP R8
    DROP R0
    DROP R0
    MOV R9, "1"
    CALL Exit, R10, R9
    DROP R10
    MOV R11, "functions"
    DROP R11
    DROP R0
    MOV R12, "string"
    DROP R12
    DROP R0
    MOV R13, "flag"
    DROP R13
    DROP R0
    MOV R14, "for"
    DROP R14
    DROP R0
    MOV R15, "range"
    DROP R15
    DROP R0
    MOV R16, "i"
    DROP R16
    DROP R0
    DROP R0
    DROP R0
    DROP R0
    DROP R0
    MOV R17, "os"
    DROP R17
    DROP R0
    DROP R0
    MOV R18, "err"
    DROP R18
    DROP R0
    MOV R19, "os"
    CALL Fprintln, R20, R19
    DROP R20
    DROP R0
    DROP R0
    DROP R0
    MOV R21, "data"
    CALL string, R22, R21
    CALL rune, R23, R22
    DROP R23
    MOV R24, "0"
    DROP R24
    DROP R0
    MOV R25, "for"
    DROP R25
    MOV R26, "EOF"
    DROP R26
    MOV R27, "if"
    DROP R27
    MOV R28, "PACKAGE"
    DROP R28
    MOV R29, "for"
    DROP R29
    MOV R30, "SEMICOLON"
    DROP R30
    MOV R31, "EOF"
    DROP R31
    MOV R32, "FUNC"
    DROP R32
    CALL next, R33
    DROP R33
    MOV R34, "if"
    DROP R34
    MOV R35, "SEMICOLON"
    DROP R35
    CALL next, R36
    DROP R36
    MOV R37, "continue"
    DROP R37
    MOV R38, "if"
    DROP R38
    MOV R39, "FUNC"
    DROP R39
    CALL next, R40
    DROP R40
    MOV R41, "lit"
    DROP R41
    DROP R0
    MOV R42, "if"
    DROP R42
    MOV R43, "LPAREN"
    DROP R43
    MOV R44, "for"
    DROP R44
    MOV R45, "RPAREN"
    DROP R45
    MOV R46, "EOF"
    DROP R46
    CALL next, R47
    DROP R47
    MOV R48, "if"
    DROP R48
    MOV R49, "RPAREN"
    DROP R49
    CALL next, R50
    DROP R50
    DROP R0
    MOV R51, "tk"
    DROP R51
    DROP R0
    MOV R52, "pos"
    DROP R52
    MOV R53, "1"
    DROP R53
    MOV R54, "pos"
    DROP R54
    DROP R0
    DROP R0
    MOV R55, "0"
    DROP R55
    MOV R56, "ch"
    DROP R56
    MOV R57, "pos"
    DROP R57
    MOV R58, "if"
    DROP R58
    DROP R0
    MOV R59, "depth"
    DROP R59
    MOV R60, "if"
    DROP R60
    DROP R0
    MOV R61, "depth"
    DROP R61
    DROP R0
    MOV R62, "src"
    DROP R62
    MOV R63, "pos"
    DROP R63
    MOV R64, "savedSrc"
    DROP R64
    MOV R65, "savedPos"
    DROP R65
    DROP R0
    MOV R66, "src"
    DROP R66
    MOV R67, "pos"
    DROP R67
    CALL next, R68
    DROP R68
    MOV R69, "instr"
    DROP R69
    DROP R0
    MOV R70, "ops"
    DROP R70
    MOV R71, "0"
    DROP R71
    MOV R72, "tk"
    DROP R72
    DROP R0
    DROP R0
    DROP R0
    MOV R73, "fname"
    DROP R73
    DROP R0
    MOV R74, "ops"
    DROP R74
    MOV R75, "regs"
    DROP R75
    DROP R0
    MOV R76, "savedSrc"
    DROP R76
    MOV R77, "savedPos"
    DROP R77
    DROP R0
    MOV R78, "continue"
    DROP R78
    DROP R0
    DROP R0
    DROP R0
    MOV R79, "if"
    DROP R79
    DROP R0
    MOV R80, "name"
    DROP R80
    MOV R81, "fn"
    DROP R81
    MOV R82, "functions"
    DROP R82
    MOV R83, "fmt"
    DROP R83
    DROP R0
    DROP R0
    DROP R0
    MOV R84, "_"
    DROP R84
    MOV R85, "ins"
    DROP R85
    MOV R86, "fn"
    DROP R86
    DROP R0
    MOV R87, "    %s\\n"
    MOV R88, "ins"
    CALL Printf, R89, R87
    DROP R89
    DROP R0
    DROP R0
    DROP R0
    DROP R0
    MOV R90, "run"
    DROP R90
    DROP R0
    DROP R0
    MOV R91, "run"
    DROP R91
    DROP R0
    MOV R92, "string"
    DROP R92
    MOV R93, "string"
    DROP R93
    MOV R94, "fn"
    DROP R94
    MOV R95, "ok"
    DROP R95
    MOV R96, "fname"
    DROP R96
    MOV R97, "ok"
    DROP R97
    MOV R98, ""
    RET R98
    DROP R0
    MOV R99, "string"
    MOV R100, "fn"
    CALL make, R101, R99
    DROP R101
    MOV R102, "10"
    DROP R102
    MOV R103, "ip"
    DROP R103
    MOV R104, "for"
    DROP R104
    MOV R105, "fn"
    CALL len, R106, R105
    DROP R106
    DROP R0
    MOV R107, "ins"
    DROP R107
    MOV R108, "code"
    DROP R108
    MOV R109, "switch"
    DROP R109
    MOV R110, "op"
    DROP R110
    MOV R111, "case"
    DROP R111
    MOV R112, "regs"
    DROP R112
    MOV R113, "r"
    DROP R113
    MOV R114, "s"
    DROP R114
    MOV R115, "PRINT"
    DROP R115
    MOV R116, "ins"
    DROP R116
    MOV R117, "regs"
    CALL len, R118, R117
    DROP R118
    MOV R119, "fmt"
    DROP R119
    DROP R0
    MOV R120, "ins"
    DROP R120
    DROP R0
    MOV R121, "else"
    DROP R121
    MOV R122, "fmt"
    DROP R122
    DROP R0
    DROP R0
    MOV R123, "CALL"
    DROP R123
    MOV R124, "ins"
    DROP R124
    MOV R125, "ins"
    CALL run, R126, R125
    DROP R126
    DROP R0
    MOV R127, "RET"
    DROP R127
    MOV R128, "ins"
    DROP R128
    MOV R129, "regs"
    CALL len, R130, R129
    DROP R130
    MOV R131, "regs"
    RET R131
    MOV R132, "ins"
    DROP R132
    DROP R0
    MOV R133, ""
    DROP R133
    MOV R134, "ip"
    DROP R134
    MOV R135, ""
    RET R135
    DROP R0
    DROP R0
    DROP R0
