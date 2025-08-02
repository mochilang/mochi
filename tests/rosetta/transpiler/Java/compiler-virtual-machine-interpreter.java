public class Main {

    static int parseIntStr(String str) {
        int i = 0;
        boolean neg = false;
        if (_runeLen(str) > 0 && (str.substring(0, 1).equals("-"))) {
            neg = true;
            i = 1;
        }
        int n = 0;
        java.util.Map<String,Integer> digits = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1), java.util.Map.entry("2", 2), java.util.Map.entry("3", 3), java.util.Map.entry("4", 4), java.util.Map.entry("5", 5), java.util.Map.entry("6", 6), java.util.Map.entry("7", 7), java.util.Map.entry("8", 8), java.util.Map.entry("9", 9)))));
        while (i < _runeLen(str)) {
            n = n * 10 + (int)(((int)(digits).get(str.substring(i, i + 1))));
            i = i + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }

    static String[] fields(String s) {
        String[] words = ((String[])(new String[]{}));
        String cur = "";
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String ch = _substr(s, i_1, i_1 + 1);
            if ((ch.equals(" ")) || (ch.equals("\t")) || (ch.equals("\n"))) {
                if (_runeLen(cur) > 0) {
                    words = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(cur)).toArray(String[]::new)));
                    cur = "";
                }
            } else {
                cur = cur + ch;
            }
            i_1 = i_1 + 1;
        }
        if (_runeLen(cur) > 0) {
            words = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(cur)).toArray(String[]::new)));
        }
        return words;
    }

    static String unescape(String s) {
        String out = "";
        int i_2 = 0;
        while (i_2 < _runeLen(s)) {
            if ((s.substring(i_2, i_2 + 1).equals("\\")) && i_2 + 1 < _runeLen(s)) {
                String c = s.substring(i_2 + 1, i_2 + 2);
                if ((c.equals("n"))) {
                    out = out + "\n";
                    i_2 = i_2 + 2;
                    continue;
                } else                 if ((c.equals("\\"))) {
                    out = out + "\\";
                    i_2 = i_2 + 2;
                    continue;
                }
            }
            out = out + s.substring(i_2, i_2 + 1);
            i_2 = i_2 + 1;
        }
        return out;
    }

    static java.util.Map<String,Object> parseProgram(String src) {
        String[] lines = ((String[])(src.split(java.util.regex.Pattern.quote("\n"))));
        String[] header = ((String[])(fields(lines[0])));
        int dataSize = Integer.parseInt(header[1]);
        int nStrings = Integer.parseInt(header[3]);
        String[] stringPool = ((String[])(new String[]{}));
        int i_3 = 1;
        while (i_3 <= nStrings) {
            String s = lines[i_3];
            if (_runeLen(s) > 0) {
                stringPool = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stringPool), java.util.stream.Stream.of(unescape(s.substring(1, _runeLen(s) - 1)))).toArray(String[]::new)));
            }
            i_3 = i_3 + 1;
        }
        java.util.Map<String,Object>[] code = ((java.util.Map<String,Object>[])((java.util.Map<String,Object>[])new java.util.Map[]{}));
        java.util.Map<Integer,Integer> addrMap = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
        while (i_3 < lines.length) {
            String line = String.valueOf(trim(lines[i_3]));
            if (_runeLen(line) == 0) {
                break;
            }
            String[] parts = ((String[])(fields(line)));
            int addr = Integer.parseInt(parts[0]);
            String op = parts[1];
            int arg = 0;
            if ((op.equals("push"))) {
                arg = Integer.parseInt(parts[2]);
            } else             if ((op.equals("fetch")) || (op.equals("store"))) {
                arg = Integer.parseInt(parts[2].substring(1, parts[2].length() - 1));
            } else             if ((op.equals("jmp")) || (op.equals("jz"))) {
                arg = Integer.parseInt(parts[3]);
            }
            code = ((java.util.Map<String,Object>[])(appendObj(code, new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("addr", addr), java.util.Map.entry("op", op), java.util.Map.entry("arg", arg))))));
addrMap.put(addr, code.length - 1);
            i_3 = i_3 + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("dataSize", dataSize), java.util.Map.entry("strings", stringPool), java.util.Map.entry("code", code), java.util.Map.entry("addrMap", addrMap)));
    }

    static void runVM(java.util.Map<String,Object> prog) {
        int[] data = ((int[])(new int[]{}));
        int i_4 = 0;
        while (i_4 < ((Number)(((Object)(prog).get("dataSize")))).intValue()) {
            data = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(data), java.util.stream.IntStream.of(0)).toArray()));
            i_4 = i_4 + 1;
        }
        int[] stack = ((int[])(new int[]{}));
        int pc = 0;
        Object code_1 = (Object)(((Object)(prog).get("code")));
        Object addrMap_1 = (Object)(((Object)(prog).get("addrMap")));
        Object pool = (Object)(((Object)(prog).get("strings")));
        String line_1 = "";
        while (pc < String.valueOf(code_1).length()) {
            Object inst = code_1[pc];
            Object op_1 = (Object)(((Object)(((java.util.Map)inst)).get("op")));
            Object arg_1 = (Object)(((Object)(((java.util.Map)inst)).get("arg")));
            if (((Number)(op_1)).intValue() == "push") {
                stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(((Number)(arg_1)).intValue())).toArray()));
                pc = pc + 1;
                continue;
            }
            if (((Number)(op_1)).intValue() == "store") {
data[arg_1] = stack[stack.length - 1];
                stack = ((int[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
                pc = pc + 1;
                continue;
            }
            if (((Number)(op_1)).intValue() == "fetch") {
                stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(data[arg_1])).toArray()));
                pc = pc + 1;
                continue;
            }
            if (((Number)(op_1)).intValue() == "add") {
stack[stack.length - 2] = stack[stack.length - 2] + stack[stack.length - 1];
                stack = ((int[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
                pc = pc + 1;
                continue;
            }
            if (((Number)(op_1)).intValue() == "lt") {
                int v = 0;
                if (stack[stack.length - 2] < stack[stack.length - 1]) {
                    v = 1;
                }
stack[stack.length - 2] = v;
                stack = ((int[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
                pc = pc + 1;
                continue;
            }
            if (((Number)(op_1)).intValue() == "jz") {
                int v_1 = stack[stack.length - 1];
                stack = ((int[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
                if (v_1 == 0) {
                    pc = ((Number)(addrMap_1[arg_1])).intValue();
                } else {
                    pc = pc + 1;
                }
                continue;
            }
            if (((Number)(op_1)).intValue() == "jmp") {
                pc = ((Number)(addrMap_1[arg_1])).intValue();
                continue;
            }
            if (((Number)(op_1)).intValue() == "prts") {
                Object s_1 = pool[stack[stack.length - 1]];
                stack = ((int[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
                if (((Number)(s_1)).intValue() != "\n") {
                    line_1 = line_1 + (String)(s_1);
                }
                pc = pc + 1;
                continue;
            }
            if (((Number)(op_1)).intValue() == "prti") {
                line_1 = line_1 + _p(_geti(stack, stack.length - 1));
                System.out.println(line_1);
                line_1 = "";
                stack = ((int[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
                pc = pc + 1;
                continue;
            }
            if (((Number)(op_1)).intValue() == "halt") {
                break;
            }
            pc = pc + 1;
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
        String[] parts_1 = ((String[])(new String[]{}));
        String cur_1 = "";
        int i_5 = 0;
        while (i_5 < _runeLen(s)) {
            if (_runeLen(sep) > 0 && i_5 + _runeLen(sep) <= _runeLen(s) && (_substr(s, i_5, i_5 + _runeLen(sep)).equals(sep))) {
                parts_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts_1), java.util.stream.Stream.of(cur_1)).toArray(String[]::new)));
                cur_1 = "";
                i_5 = i_5 + _runeLen(sep);
            } else {
                cur_1 = cur_1 + _substr(s, i_5, i_5 + 1);
                i_5 = i_5 + 1;
            }
        }
        parts_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts_1), java.util.stream.Stream.of(cur_1)).toArray(String[]::new)));
        return parts_1;
    }

    static void main() {
        String programText = "Datasize: 1 Strings: 2\n" + "\"count is: \"\n" + "\"\\n\"\n" + "    0 push  1\n" + "    5 store [0]\n" + "   10 fetch [0]\n" + "   15 push  10\n" + "   20 lt\n" + "   21 jz     (43) 65\n" + "   26 push  0\n" + "   31 prts\n" + "   32 fetch [0]\n" + "   37 prti\n" + "   38 push  1\n" + "   43 prts\n" + "   44 fetch [0]\n" + "   49 push  1\n" + "   54 add\n" + "   55 store [0]\n" + "   60 jmp    (-51) 10\n" + "   65 halt\n";
        java.util.Map<String,Object> prog = parseProgram(programText);
        runVM(prog);
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
