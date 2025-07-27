public class Main {

    static int parseIntStr(String str) {
        int i = 0;
        boolean neg = false;
        if (str.length() > 0 && (str.substring(0, 1).equals("-"))) {
            neg = true;
            i = 1;
        }
        int n = 0;
        java.util.Map<String,Integer> digits = new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1), java.util.Map.entry("2", 2), java.util.Map.entry("3", 3), java.util.Map.entry("4", 4), java.util.Map.entry("5", 5), java.util.Map.entry("6", 6), java.util.Map.entry("7", 7), java.util.Map.entry("8", 8), java.util.Map.entry("9", 9)));
        while (i < str.length()) {
            n = n * 10 + (int)(((int)digits.getOrDefault(str.substring(i, i + 1), 0)));
            i = i + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }

    static String[] fields(String s) {
        String[] words = new String[]{};
        String cur = "";
        int i = 0;
        while (i < s.length()) {
            String ch = s.substring(i, i + 1);
            if ((ch.equals(" ")) || (ch.equals("\t")) || (ch.equals("\n"))) {
                if (cur.length() > 0) {
                    words = java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                    cur = "";
                }
            } else {
                cur = cur + ch;
            }
            i = i + 1;
        }
        if (cur.length() > 0) {
            words = java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        }
        return words;
    }

    static String unescape(String s) {
        String out = "";
        int i = 0;
        while (i < s.length()) {
            if ((s.substring(i, i + 1).equals("\\")) && i + 1 < s.length()) {
                String c = s.substring(i + 1, i + 2);
                if ((c.equals("n"))) {
                    out = out + "\n";
                    i = i + 2;
                    continue;
                } else                 if ((c.equals("\\"))) {
                    out = out + "\\";
                    i = i + 2;
                    continue;
                }
            }
            out = out + s.substring(i, i + 1);
            i = i + 1;
        }
        return out;
    }

    static java.util.Map<String,Object> parseProgram(String src) {
        String[] lines = split(src, "\n");
        String[] header = fields(lines[0]);
        int dataSize = Integer.parseInt(header[1]);
        int nStrings = Integer.parseInt(header[3]);
        String[] stringPool = new String[]{};
        int i = 1;
        while (i <= nStrings) {
            String s = lines[i];
            if (s.length() > 0) {
                stringPool = java.util.stream.Stream.concat(java.util.Arrays.stream(stringPool), java.util.stream.Stream.of(unescape(s.substring(1, s.length() - 1)))).toArray(String[]::new);
            }
            i = i + 1;
        }
        java.util.Map<String,Object>[] code = (java.util.Map<String,Object>[])new java.util.Map[]{};
        java.util.Map<Integer,Integer> addrMap = new java.util.LinkedHashMap<Integer, Integer>();
        while (i < lines.length) {
            String line = String.valueOf(trim(lines[i]));
            if (line.length() == 0) {
                break;
            }
            String[] parts = fields(line);
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
            code = appendObj(code, new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("addr", addr), java.util.Map.entry("op", op), java.util.Map.entry("arg", arg))));
addrMap.put(addr, code.length - 1);
            i = i + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("dataSize", dataSize), java.util.Map.entry("strings", stringPool), java.util.Map.entry("code", code), java.util.Map.entry("addrMap", addrMap)));
    }

    static void runVM(java.util.Map<String,Object> prog) {
        int[] data = new int[]{};
        int i = 0;
        while (i < ((Number)(((Object)prog.get("dataSize")))).intValue()) {
            data = java.util.stream.IntStream.concat(java.util.Arrays.stream(data), java.util.stream.IntStream.of(0)).toArray();
            i = i + 1;
        }
        int[] stack = new int[]{};
        int pc = 0;
        Object code = (Object)(((Object)prog.get("code")));
        Object addrMap = (Object)(((Object)prog.get("addrMap")));
        Object pool = (Object)(((Object)prog.get("strings")));
        String line = "";
        while (pc < String.valueOf(code).length()) {
            Object inst = code[pc];
            Object op = (Object)(((Object)((java.util.Map)inst).get("op")));
            Object arg = (Object)(((Object)((java.util.Map)inst).get("arg")));
            if (((Number)(op)).intValue() == "push") {
                stack = java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(((Number)(arg)).intValue())).toArray();
                pc = pc + 1;
                continue;
            }
            if (((Number)(op)).intValue() == "store") {
data[arg] = stack[stack.length - 1];
                stack = slice(stack, 0, stack.length - 1);
                pc = pc + 1;
                continue;
            }
            if (((Number)(op)).intValue() == "fetch") {
                stack = java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(data[arg])).toArray();
                pc = pc + 1;
                continue;
            }
            if (((Number)(op)).intValue() == "add") {
stack[stack.length - 2] = stack[stack.length - 2] + stack[stack.length - 1];
                stack = slice(stack, 0, stack.length - 1);
                pc = pc + 1;
                continue;
            }
            if (((Number)(op)).intValue() == "lt") {
                int v = 0;
                if (stack[stack.length - 2] < stack[stack.length - 1]) {
                    v = 1;
                }
stack[stack.length - 2] = v;
                stack = slice(stack, 0, stack.length - 1);
                pc = pc + 1;
                continue;
            }
            if (((Number)(op)).intValue() == "jz") {
                int v = stack[stack.length - 1];
                stack = slice(stack, 0, stack.length - 1);
                if (v == 0) {
                    pc = ((Number)(addrMap[arg])).intValue();
                } else {
                    pc = pc + 1;
                }
                continue;
            }
            if (((Number)(op)).intValue() == "jmp") {
                pc = ((Number)(addrMap[arg])).intValue();
                continue;
            }
            if (((Number)(op)).intValue() == "prts") {
                Object s = pool[stack[stack.length - 1]];
                stack = slice(stack, 0, stack.length - 1);
                if (((Number)(s)).intValue() != "\n") {
                    line = line + (String)(s);
                }
                pc = pc + 1;
                continue;
            }
            if (((Number)(op)).intValue() == "prti") {
                line = line + String.valueOf(stack[stack.length - 1]);
                System.out.println(line);
                line = "";
                stack = slice(stack, 0, stack.length - 1);
                pc = pc + 1;
                continue;
            }
            if (((Number)(op)).intValue() == "halt") {
                break;
            }
            pc = pc + 1;
        }
    }

    static String trim(String s) {
        int start = 0;
        while (start < s.length() && ((s.substring(start, start + 1).equals(" ")) || (s.substring(start, start + 1).equals("\t")))) {
            start = start + 1;
        }
        int end = s.length();
        while (end > start && ((s.substring(end - 1, end).equals(" ")) || (s.substring(end - 1, end).equals("\t")))) {
            end = end - 1;
        }
        return s.substring(start, end);
    }

    static String[] split(String s, String sep) {
        String[] parts = new String[]{};
        String cur = "";
        int i = 0;
        while (i < s.length()) {
            if (sep.length() > 0 && i + sep.length() <= s.length() && (s.substring(i, i + sep.length()).equals(sep))) {
                parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                cur = "";
                i = i + sep.length();
            } else {
                cur = cur + s.substring(i, i + 1);
                i = i + 1;
            }
        }
        parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        return parts;
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
}
