public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static java.math.BigInteger parseIntStr(String str) {
        java.util.Map<String,java.math.BigInteger> digits = ((java.util.Map<String,java.math.BigInteger>)(new java.util.LinkedHashMap<String, java.math.BigInteger>(java.util.Map.of("0", java.math.BigInteger.valueOf(0), "1", java.math.BigInteger.valueOf(1), "2", java.math.BigInteger.valueOf(2), "3", java.math.BigInteger.valueOf(3), "4", java.math.BigInteger.valueOf(4), "5", java.math.BigInteger.valueOf(5), "6", java.math.BigInteger.valueOf(6), "7", java.math.BigInteger.valueOf(7), "8", java.math.BigInteger.valueOf(8), "9", java.math.BigInteger.valueOf(9)))));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger n_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(str)))) < 0) {
            n_1 = n_1.multiply(java.math.BigInteger.valueOf(10)).add(new java.math.BigInteger(String.valueOf((((Number)(((java.math.BigInteger)(digits).get(_substr(str, (int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1.add(java.math.BigInteger.valueOf(1)))).longValue())))))).intValue()))));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return n_1;
    }

    static java.math.BigInteger precedence(String op) {
        if ((String.valueOf(op).equals(String.valueOf("+"))) || (String.valueOf(op).equals(String.valueOf("-")))) {
            return java.math.BigInteger.valueOf(1);
        }
        if ((String.valueOf(op).equals(String.valueOf("*"))) || (String.valueOf(op).equals(String.valueOf("/")))) {
            return java.math.BigInteger.valueOf(2);
        }
        return java.math.BigInteger.valueOf(0);
    }

    static Object parse(String s) {
        String[] ops = ((String[])(new String[]{}));
        Object[] vals_1 = new Object[]{};
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(s)))) < 0) {
            String ch_1 = _substr(s, (int)(((java.math.BigInteger)(i_3)).longValue()), (int)(((java.math.BigInteger)(i_3.add(java.math.BigInteger.valueOf(1)))).longValue()));
            if ((ch_1.compareTo("a") >= 0) && (ch_1.compareTo("z") <= 0)) {
                vals_1 = java.util.stream.Stream.concat(java.util.Arrays.stream(vals_1), java.util.stream.Stream.of(new java.util.LinkedHashMap<String, Object>(java.util.Map.of("kind", (Object)("var"), "val", (Object)(ch_1))))).toArray(java.util.Map[]::new);
            } else             if ((String.valueOf(ch_1).equals(String.valueOf("(")))) {
                ops = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ops), java.util.stream.Stream.of(ch_1)).toArray(String[]::new)));
            } else             if ((String.valueOf(ch_1).equals(String.valueOf(")")))) {
                while (new java.math.BigInteger(String.valueOf(ops.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0 && !(String.valueOf(ops[_idx((ops).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(ops.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())]).equals(String.valueOf("(")))) {
                    String op_2 = ops[_idx((ops).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(ops.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())];
                    ops = ((String[])(java.util.Arrays.copyOfRange(ops, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(ops.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue()))));
                    Object right_2 = vals_1[_idx((vals_1).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(vals_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())];
                    vals_1 = java.util.Arrays.copyOfRange(vals_1, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(vals_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue()));
                    Object left_2 = vals_1[_idx((vals_1).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(vals_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())];
                    vals_1 = java.util.Arrays.copyOfRange(vals_1, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(vals_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue()));
                    vals_1 = java.util.stream.Stream.concat(java.util.Arrays.stream(vals_1), java.util.stream.Stream.of(new java.util.LinkedHashMap<String, Object>(java.util.Map.of("kind", (Object)("op"), "op_2", (Object)(op_2), "left_2", (Object)(left_2), "right_2", (Object)(right_2))))).toArray(java.util.Map[]::new);
                }
                ops = ((String[])(java.util.Arrays.copyOfRange(ops, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(ops.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue()))));
            } else {
                while (new java.math.BigInteger(String.valueOf(ops.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0 && !(String.valueOf(ops[_idx((ops).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(ops.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())]).equals(String.valueOf("("))) && precedence(ops[_idx((ops).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(ops.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())]).compareTo(precedence(ch_1)) >= 0) {
                    String op_3 = ops[_idx((ops).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(ops.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())];
                    ops = ((String[])(java.util.Arrays.copyOfRange(ops, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(ops.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue()))));
                    Object right_3 = vals_1[_idx((vals_1).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(vals_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())];
                    vals_1 = java.util.Arrays.copyOfRange(vals_1, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(vals_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue()));
                    Object left_3 = vals_1[_idx((vals_1).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(vals_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())];
                    vals_1 = java.util.Arrays.copyOfRange(vals_1, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(vals_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue()));
                    vals_1 = java.util.stream.Stream.concat(java.util.Arrays.stream(vals_1), java.util.stream.Stream.of(new java.util.LinkedHashMap<String, Object>(java.util.Map.of("kind", (Object)("op"), "op_3", (Object)(op_3), "left_3", (Object)(left_3), "right_3", (Object)(right_3))))).toArray(java.util.Map[]::new);
                }
                ops = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ops), java.util.stream.Stream.of(ch_1)).toArray(String[]::new)));
            }
            i_3 = i_3.add(java.math.BigInteger.valueOf(1));
        }
        while (new java.math.BigInteger(String.valueOf(ops.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            String op_5 = ops[_idx((ops).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(ops.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())];
            ops = ((String[])(java.util.Arrays.copyOfRange(ops, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(ops.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue()))));
            Object right_5 = vals_1[_idx((vals_1).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(vals_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())];
            vals_1 = java.util.Arrays.copyOfRange(vals_1, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(vals_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue()));
            Object left_5 = vals_1[_idx((vals_1).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(vals_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())];
            vals_1 = java.util.Arrays.copyOfRange(vals_1, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(vals_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue()));
            vals_1 = java.util.stream.Stream.concat(java.util.Arrays.stream(vals_1), java.util.stream.Stream.of(new java.util.LinkedHashMap<String, Object>(java.util.Map.of("kind", (Object)("op"), "op_5", (Object)(op_5), "left_5", (Object)(left_5), "right_5", (Object)(right_5))))).toArray(java.util.Map[]::new);
        }
        return ((Object)(vals_1[_idx((vals_1).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(vals_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
    }

    static boolean needParen(String parent, boolean isRight, Object child) {
        if (!(String.valueOf(((Object)(_getm(child, "kind")))).equals(String.valueOf("op")))) {
            return false;
        }
        java.math.BigInteger p_1 = precedence(parent);
        java.math.BigInteger c_1 = precedence((String)(((Object)(_getm(child, "op")))));
        if (c_1.compareTo(p_1) < 0) {
            return true;
        }
        if (c_1.compareTo(p_1) > 0) {
            return false;
        }
        if ((String.valueOf(parent).equals(String.valueOf("-"))) && isRight && ((String.valueOf(((Object)(_getm(child, "op")))).equals(String.valueOf("+"))) || (String.valueOf(((Object)(_getm(child, "op")))).equals(String.valueOf("-"))))) {
            return true;
        }
        if ((String.valueOf(parent).equals(String.valueOf("/"))) && isRight && ((String.valueOf(((Object)(_getm(child, "op")))).equals(String.valueOf("*"))) || (String.valueOf(((Object)(_getm(child, "op")))).equals(String.valueOf("/"))))) {
            return true;
        }
        return false;
    }

    static String formatRec(Object node, String parent, boolean isRight) {
        if (!(String.valueOf(((Object)(_getm(node, "kind")))).equals(String.valueOf("op")))) {
            return String.valueOf(((String)(((Object)(_getm(node, "val"))))));
        }
        String left_7 = String.valueOf(formatRec(((Object)(_getm(node, "left"))), (String)(((Object)(_getm(node, "op")))), false));
        String right_7 = String.valueOf(formatRec(((Object)(_getm(node, "right"))), (String)(((Object)(_getm(node, "op")))), true));
        String res_1 = left_7 + (String)(((Object)(_getm(node, "op")))) + right_7;
        if (!(String.valueOf(parent).equals(String.valueOf(""))) && needParen(parent, isRight, node)) {
            res_1 = "(" + res_1 + ")";
        }
        return res_1;
    }

    static String makeNice(String s) {
        Object root = parse(s);
        return String.valueOf(formatRec(root, "", false));
    }

    static void main() {
        String tStr = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
        if ((String.valueOf(tStr).equals(String.valueOf("")))) {
            return;
        }
        java.math.BigInteger t_1 = new java.math.BigInteger(String.valueOf(Integer.parseInt(tStr)));
        for (java.math.BigInteger __1 = java.math.BigInteger.valueOf(0); __1.compareTo(t_1) < 0; __1 = __1.add(java.math.BigInteger.ONE)) {
            String line_1 = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            System.out.println(makeNice(line_1));
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static Object _getm(Object m, String k) {
        if (!(m instanceof java.util.Map<?,?>)) return null;
        return ((java.util.Map<?,?>)m).get(k);
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
