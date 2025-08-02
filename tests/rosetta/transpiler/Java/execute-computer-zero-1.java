public class Main {
    static class Instruction {
        String Label;
        String Opcode;
        String Arg;
        Instruction(String Label, String Opcode, String Arg) {
            this.Label = Label;
            this.Opcode = Opcode;
            this.Arg = Arg;
        }
        @Override public String toString() {
            return String.format("{'Label': '%s', 'Opcode': '%s', 'Arg': '%s'}", String.valueOf(Label), String.valueOf(Opcode), String.valueOf(Arg));
        }
    }


    static String trim(String s) {
        int start = 0;
        while (start < _runeLen(s) && ((s.substring(start, start + 1).equals(" ")) || (s.substring(start, start + 1).equals("\t")))) {
            start = start + 1;
        }
        int end = _runeLen(s);
        while (end > start && ((s.substring(end - 1, end).equals(" ")) || (s.substring(end - 1, end).equals("\t")))) {
            end = end - 1;
        }
        return _substr(s, start, end);
    }

    static String[] split(String s, String sep) {
        String[] parts = new String[]{};
        String cur = "";
        int i = 0;
        while (i < _runeLen(s)) {
            if (_runeLen(sep) > 0 && i + _runeLen(sep) <= _runeLen(s) && (_substr(s, i, i + _runeLen(sep)).equals(sep))) {
                parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                cur = "";
                i = i + _runeLen(sep);
            } else {
                cur = cur + _substr(s, i, i + 1);
                i = i + 1;
            }
        }
        parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        return parts;
    }

    static String[] splitWS(String s) {
        String[] out = new String[]{};
        String cur_1 = "";
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String ch = _substr(s, i_1, i_1 + 1);
            if ((ch.equals(" ")) || (ch.equals("\t"))) {
                if (_runeLen(cur_1) > 0) {
                    out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(cur_1)).toArray(String[]::new);
                    cur_1 = "";
                }
            } else {
                cur_1 = cur_1 + ch;
            }
            i_1 = i_1 + 1;
        }
        if (_runeLen(cur_1) > 0) {
            out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(cur_1)).toArray(String[]::new);
        }
        return out;
    }

    static int indexOf(String s, String ch) {
        int i_2 = 0;
        while (i_2 < _runeLen(s)) {
            if ((_substr(s, i_2, i_2 + 1).equals(ch))) {
                return i_2;
            }
            i_2 = i_2 + 1;
        }
        return -1;
    }

    static int parseIntStr(String str) {
        int i_3 = 0;
        boolean neg = false;
        if (_runeLen(str) > 0 && (str.substring(0, 1).equals("-"))) {
            neg = true;
            i_3 = 1;
        }
        int n = 0;
        java.util.Map<String,Integer> digits = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1), java.util.Map.entry("2", 2), java.util.Map.entry("3", 3), java.util.Map.entry("4", 4), java.util.Map.entry("5", 5), java.util.Map.entry("6", 6), java.util.Map.entry("7", 7), java.util.Map.entry("8", 8), java.util.Map.entry("9", 9)))));
        while (i_3 < _runeLen(str)) {
            n = n * 10 + (int)(((int)(digits).get(str.substring(i_3, i_3 + 1))));
            i_3 = i_3 + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }

    static java.util.Map<String,Object> parseAsm(String asm) {
        String[] lines = asm.split("\n");
        Instruction[] instrs = new Instruction[]{};
        java.util.Map<String,Integer> labels = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        int lineNum = 0;
        int i_4 = 0;
        while (i_4 < lines.length) {
            String line = lines[i_4];
            if (((Number)(line.indexOf(";"))).intValue() != (-1)) {
                line = _substr(line, 0, line.indexOf(";"));
            }
            line = String.valueOf(trim(line));
            String label = "";
            if (((Number)(line.indexOf(":"))).intValue() != (-1)) {
                int idx = ((Number)(line.indexOf(":"))).intValue();
                label = String.valueOf(trim(_substr(line, 0, idx)));
                line = String.valueOf(trim(_substr(line, idx + 1, _runeLen(line))));
            }
            String opcode = "";
            String arg = "";
            if (_runeLen(line) > 0) {
                String[] parts_1 = splitWS(line);
                if (parts_1.length > 0) {
                    opcode = parts_1[0];
                }
                if (parts_1.length > 1) {
                    arg = parts_1[1];
                } else {
                    java.util.Map<String,Integer> ops = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("NOP", 0), java.util.Map.entry("LDA", 1), java.util.Map.entry("STA", 2), java.util.Map.entry("ADD", 3), java.util.Map.entry("SUB", 4), java.util.Map.entry("BRZ", 5), java.util.Map.entry("JMP", 6), java.util.Map.entry("STP", 7)))));
                    if (!(Boolean)(ops.containsKey(opcode))) {
                        arg = opcode;
                        opcode = "";
                    }
                }
            }
            if (!(label.equals(""))) {
labels.put(label, lineNum);
            }
            instrs = java.util.stream.Stream.concat(java.util.Arrays.stream(instrs), java.util.stream.Stream.of(new Instruction(label, opcode, arg))).toArray(Instruction[]::new);
            lineNum = lineNum + 1;
            i_4 = i_4 + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("instructions", instrs), java.util.Map.entry("labels", labels)));
    }

    static int[] compile(java.util.Map<String,Object> p) {
        Instruction[] instrs_1 = (Instruction[])(((Instruction[])(p).get("instructions")));
        java.util.Map<String,Integer> labels_1 = (java.util.Map<String,Integer>)(((java.util.Map<String,Integer>)(p).get("labels")));
        int[] bytecode = new int[]{};
        int i_5 = 0;
        java.util.Map<String,Integer> opcodes = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("NOP", 0), java.util.Map.entry("LDA", 1), java.util.Map.entry("STA", 2), java.util.Map.entry("ADD", 3), java.util.Map.entry("SUB", 4), java.util.Map.entry("BRZ", 5), java.util.Map.entry("JMP", 6), java.util.Map.entry("STP", 7)))));
        while (i_5 < instrs_1.length) {
            Instruction ins = instrs_1[i_5];
            int arg_1 = 0;
            if (!(ins.Arg.equals(""))) {
                if (((Boolean)(labels_1.containsKey(ins.Arg)))) {
                    arg_1 = (int)(((int)(labels_1).getOrDefault(ins.Arg, 0)));
                } else {
                    arg_1 = Integer.parseInt(ins.Arg);
                }
            }
            int code = 0;
            if (!(ins.Opcode.equals(""))) {
                code = (int)(((int)(opcodes).get(ins.Opcode)));
            }
            bytecode = java.util.stream.IntStream.concat(java.util.Arrays.stream(bytecode), java.util.stream.IntStream.of(code * 32 + arg_1)).toArray();
            i_5 = i_5 + 1;
        }
        while (bytecode.length < 32) {
            bytecode = java.util.stream.IntStream.concat(java.util.Arrays.stream(bytecode), java.util.stream.IntStream.of(0)).toArray();
        }
        return bytecode;
    }

    static int floorMod(int a, int b) {
        int r = Math.floorMod(a, b);
        if (r < 0) {
            r = r + b;
        }
        return r;
    }

    static int run(int[] bytecode) {
        int acc = 0;
        int pc = 0;
        int[] mem = new int[]{};
        int i_6 = 0;
        while (i_6 < bytecode.length) {
            mem = java.util.stream.IntStream.concat(java.util.Arrays.stream(mem), java.util.stream.IntStream.of(bytecode[i_6])).toArray();
            i_6 = i_6 + 1;
        }
        while (pc < 32) {
            int op = mem[pc] / 32;
            int arg_2 = Math.floorMod(mem[pc], 32);
            pc = pc + 1;
            if (op == 0) {
                continue;
            } else             if (op == 1) {
                acc = mem[arg_2];
            } else             if (op == 2) {
mem[arg_2] = acc;
            } else             if (op == 3) {
                acc = floorMod(acc + mem[arg_2], 256);
            } else             if (op == 4) {
                acc = floorMod(acc - mem[arg_2], 256);
            } else             if (op == 5) {
                if (acc == 0) {
                    pc = arg_2;
                }
            } else             if (op == 6) {
                pc = arg_2;
            } else             if (op == 7) {
                break;
            } else {
                break;
            }
        }
        return acc;
    }

    static int execute(String asm) {
        java.util.Map<String,Object> parsed = parseAsm(asm);
        int[] bc = compile(parsed);
        return run(bc);
    }

    static void main() {
        String[] examples = new String[]{"LDA   x\n" + "ADD   y       ; accumulator = x + y\n" + "STP\n" + "x:            2\n" + "y:            2", "loop:   LDA   prodt\n" + "        ADD   x\n" + "        STA   prodt\n" + "        LDA   y\n" + "        SUB   one\n" + "        STA   y\n" + "        BRZ   done\n" + "        JMP   loop\n" + "done:   LDA   prodt   ; to display it\n" + "        STP\n" + "x:            8\n" + "y:            7\n" + "prodt:        0\n" + "one:          1", "loop:   LDA   n\n" + "        STA   temp\n" + "        ADD   m\n" + "        STA   n\n" + "        LDA   temp\n" + "        STA   m\n" + "        LDA   count\n" + "        SUB   one\n" + "        BRZ   done\n" + "        STA   count\n" + "        JMP   loop\n" + "done:   LDA   n       ; to display it\n" + "        STP\n" + "m:            1\n" + "n:            1\n" + "temp:         0\n" + "count:        8       ; valid range: 1-11\n" + "one:          1", "start:  LDA   load\n" + "ADD   car     ; head of list\n" + "STA   ldcar\n" + "ADD   one\n" + "STA   ldcdr   ; next CONS cell\n" + "ldcar:  NOP\n" + "STA   value\n" + "ldcdr:  NOP\n" + "BRZ   done    ; 0 stands for NIL\n" + "STA   car\n" + "JMP   start\n" + "done:   LDA   value   ; CAR of last CONS\n" + "STP\n" + "load:   LDA   0\n" + "value:        0\n" + "car:          28\n" + "one:          1\n" + "                        ; order of CONS cells\n" + "                        ; in memory\n" + "                        ; does not matter\n" + "        6\n" + "        0       ; 0 stands for NIL\n" + "        2       ; (CADR ls)\n" + "        26      ; (CDDR ls) -- etc.\n" + "        5\n" + "        20\n" + "        3\n" + "        30\n" + "        1       ; value of (CAR ls)\n" + "        22      ; points to (CDR ls)\n" + "        4\n" + "        24", "LDA  3\n" + "SUB  4\n" + "STP  0\n" + "         0\n" + "         255", "LDA  3\n" + "SUB  4\n" + "STP  0\n" + "                0\n" + "                1", "LDA  3\n" + "ADD  4\n" + "STP  0\n" + "                1\n" + "                255"};
        int i_7 = 0;
        while (i_7 < examples.length) {
            int res = execute(examples[i_7]);
            System.out.println(_p(res));
            i_7 = i_7 + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
