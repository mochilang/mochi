public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static String[] split(String s, String sep) {
        String[] parts = ((String[])(new String[]{}));
        String cur_1 = "";
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(s)))) < 0) {
            if (new java.math.BigInteger(String.valueOf(_runeLen(sep))).compareTo(java.math.BigInteger.valueOf(0)) > 0 && i_1.add(new java.math.BigInteger(String.valueOf(_runeLen(sep)))).compareTo(new java.math.BigInteger(String.valueOf(_runeLen(s)))) <= 0 && (String.valueOf(_substr(s, (int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1.add(new java.math.BigInteger(String.valueOf(_runeLen(sep)))))).longValue()))).equals(String.valueOf(sep)))) {
                parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur_1)).toArray(String[]::new)));
                cur_1 = "";
                i_1 = i_1.add(new java.math.BigInteger(String.valueOf(_runeLen(sep))));
            } else {
                cur_1 = cur_1 + _substr(s, (int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1.add(java.math.BigInteger.valueOf(1)))).longValue()));
                i_1 = i_1.add(java.math.BigInteger.valueOf(1));
            }
        }
        parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur_1)).toArray(String[]::new)));
        return ((String[])(parts));
    }

    static java.math.BigInteger[] parse_ints(String line) {
        String[] pieces = ((String[])(line.split(java.util.regex.Pattern.quote(" "))));
        java.math.BigInteger[] nums_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        for (String p : pieces) {
            if (new java.math.BigInteger(String.valueOf(_runeLen(p))).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                nums_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nums_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(Integer.parseInt(p))))).toArray(java.math.BigInteger[]::new)));
            }
        }
        return ((java.math.BigInteger[])(nums_1));
    }

    static void main() {
        String tLine = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
        if ((String.valueOf(tLine).equals(String.valueOf("")))) {
            return;
        }
        java.math.BigInteger t_1 = new java.math.BigInteger(String.valueOf(Integer.parseInt(tLine)));
        java.math.BigInteger caseIdx_1 = java.math.BigInteger.valueOf(0);
        while (caseIdx_1.compareTo(t_1) < 0) {
            java.math.BigInteger[] header_1 = ((java.math.BigInteger[])(parse_ints((_scanner.hasNextLine() ? _scanner.nextLine() : ""))));
            java.math.BigInteger s_1 = header_1[_idx((header_1).length, 0L)];
            java.math.BigInteger c_1 = header_1[_idx((header_1).length, 1L)];
            java.math.BigInteger[] seq_1 = ((java.math.BigInteger[])(parse_ints((_scanner.hasNextLine() ? _scanner.nextLine() : ""))));
            java.math.BigInteger[][] levels_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
            levels_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(levels_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(seq_1))})).toArray(java.math.BigInteger[][]::new)));
            java.math.BigInteger[] current_1 = ((java.math.BigInteger[])(seq_1));
            while (new java.math.BigInteger(String.valueOf(current_1.length)).compareTo(java.math.BigInteger.valueOf(1)) > 0) {
                java.math.BigInteger[] next_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
                java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
                while (i_3.add(java.math.BigInteger.valueOf(1)).compareTo(new java.math.BigInteger(String.valueOf(current_1.length))) < 0) {
                    next_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(next_1), java.util.stream.Stream.of(current_1[_idx((current_1).length, ((java.math.BigInteger)(i_3.add(java.math.BigInteger.valueOf(1)))).longValue())].subtract(current_1[_idx((current_1).length, ((java.math.BigInteger)(i_3)).longValue())]))).toArray(java.math.BigInteger[]::new)));
                    i_3 = i_3.add(java.math.BigInteger.valueOf(1));
                }
                levels_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(levels_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(next_1))})).toArray(java.math.BigInteger[][]::new)));
                current_1 = ((java.math.BigInteger[])(next_1));
            }
            java.math.BigInteger depth_1 = new java.math.BigInteger(String.valueOf(levels_1.length)).subtract(java.math.BigInteger.valueOf(1));
            java.math.BigInteger step_1 = java.math.BigInteger.valueOf(0);
            java.math.BigInteger[] res_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            while (step_1.compareTo(c_1) < 0) {
                java.math.BigInteger[] bottom_1 = ((java.math.BigInteger[])(levels_1[_idx((levels_1).length, ((java.math.BigInteger)(depth_1)).longValue())]));
                bottom_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(bottom_1), java.util.stream.Stream.of(bottom_1[_idx((bottom_1).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(bottom_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())])).toArray(java.math.BigInteger[]::new)));
levels_1[(int)(((java.math.BigInteger)(depth_1)).longValue())] = ((java.math.BigInteger[])(bottom_1));
                java.math.BigInteger level_1 = depth_1.subtract(java.math.BigInteger.valueOf(1));
                while (level_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
                    java.math.BigInteger[] arr_1 = ((java.math.BigInteger[])(levels_1[_idx((levels_1).length, ((java.math.BigInteger)(level_1)).longValue())]));
                    java.math.BigInteger[] arrBelow_1 = ((java.math.BigInteger[])(levels_1[_idx((levels_1).length, ((java.math.BigInteger)(level_1.add(java.math.BigInteger.valueOf(1)))).longValue())]));
                    java.math.BigInteger nextVal_1 = arr_1[_idx((arr_1).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(arr_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())].add(arrBelow_1[_idx((arrBelow_1).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(arrBelow_1.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())]);
                    arr_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(nextVal_1)).toArray(java.math.BigInteger[]::new)));
levels_1[(int)(((java.math.BigInteger)(level_1)).longValue())] = ((java.math.BigInteger[])(arr_1));
                    level_1 = level_1.subtract(java.math.BigInteger.valueOf(1));
                }
                res_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(levels_1[_idx((levels_1).length, 0L)][_idx((levels_1[_idx((levels_1).length, 0L)]).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(levels_1[_idx((levels_1).length, 0L)].length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())])).toArray(java.math.BigInteger[]::new)));
                step_1 = step_1.add(java.math.BigInteger.valueOf(1));
            }
            String out_1 = "";
            java.math.BigInteger i2_1 = java.math.BigInteger.valueOf(0);
            while (i2_1.compareTo(new java.math.BigInteger(String.valueOf(res_1.length))) < 0) {
                if (i2_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                    out_1 = out_1 + " ";
                }
                out_1 = out_1 + _p(_geto(res_1, ((Number)(i2_1)).intValue()));
                i2_1 = i2_1.add(java.math.BigInteger.valueOf(1));
            }
            System.out.println(out_1);
            caseIdx_1 = caseIdx_1.add(java.math.BigInteger.valueOf(1));
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

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }

    static Object _getm(Object m, String k) {
        if (!(m instanceof java.util.Map<?,?>)) return null;
        return ((java.util.Map<?,?>)m).get(k);
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
