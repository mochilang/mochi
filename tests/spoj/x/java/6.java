public class Main {
    static java.util.Map<String,java.math.BigInteger> digitMap;

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static String repeat(String s, java.math.BigInteger n) {
        String r = "";
        for (java.math.BigInteger __1 = java.math.BigInteger.valueOf(0); __1.compareTo(n) < 0; __1 = __1.add(java.math.BigInteger.ONE)) {
            r = r + s;
        }
        return r;
    }

    static String add_str(String a, String b) {
        java.math.BigInteger i = new java.math.BigInteger(String.valueOf(_runeLen(a))).subtract(java.math.BigInteger.valueOf(1));
        java.math.BigInteger j_1 = new java.math.BigInteger(String.valueOf(_runeLen(b))).subtract(java.math.BigInteger.valueOf(1));
        java.math.BigInteger carry_1 = java.math.BigInteger.valueOf(0);
        String res_1 = "";
        while (i.compareTo(java.math.BigInteger.valueOf(0)) >= 0 || j_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0 || carry_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            java.math.BigInteger da_1 = java.math.BigInteger.valueOf(0);
            if (i.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
                da_1 = new java.math.BigInteger(String.valueOf(((Number)(((java.math.BigInteger)(digitMap).get(_substr(a, (int)(((java.math.BigInteger)(i)).longValue()), (int)(((java.math.BigInteger)(i.add(java.math.BigInteger.valueOf(1)))).longValue())))))).intValue()));
            }
            java.math.BigInteger db_1 = java.math.BigInteger.valueOf(0);
            if (j_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
                db_1 = new java.math.BigInteger(String.valueOf(((Number)(((java.math.BigInteger)(digitMap).get(_substr(b, (int)(((java.math.BigInteger)(j_1)).longValue()), (int)(((java.math.BigInteger)(j_1.add(java.math.BigInteger.valueOf(1)))).longValue())))))).intValue()));
            }
            java.math.BigInteger sum_1 = da_1.add(db_1).add(carry_1);
            res_1 = _p(sum_1.remainder(java.math.BigInteger.valueOf(10))) + res_1;
            carry_1 = sum_1.divide(java.math.BigInteger.valueOf(10));
            i = i.subtract(java.math.BigInteger.valueOf(1));
            j_1 = j_1.subtract(java.math.BigInteger.valueOf(1));
        }
        return res_1;
    }

    static String sub_str(String a, String b) {
        java.math.BigInteger i_1 = new java.math.BigInteger(String.valueOf(_runeLen(a))).subtract(java.math.BigInteger.valueOf(1));
        java.math.BigInteger j_3 = new java.math.BigInteger(String.valueOf(_runeLen(b))).subtract(java.math.BigInteger.valueOf(1));
        java.math.BigInteger borrow_1 = java.math.BigInteger.valueOf(0);
        String res_3 = "";
        while (i_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
            java.math.BigInteger da_3 = new java.math.BigInteger(String.valueOf(((Number)(((java.math.BigInteger)(digitMap).get(_substr(a, (int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1.add(java.math.BigInteger.valueOf(1)))).longValue())))))).intValue())).subtract(borrow_1);
            java.math.BigInteger db_3 = java.math.BigInteger.valueOf(0);
            if (j_3.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
                db_3 = new java.math.BigInteger(String.valueOf(((Number)(((java.math.BigInteger)(digitMap).get(_substr(b, (int)(((java.math.BigInteger)(j_3)).longValue()), (int)(((java.math.BigInteger)(j_3.add(java.math.BigInteger.valueOf(1)))).longValue())))))).intValue()));
            }
            if (da_3.compareTo(db_3) < 0) {
                da_3 = da_3.add(java.math.BigInteger.valueOf(10));
                borrow_1 = java.math.BigInteger.valueOf(1);
            } else {
                borrow_1 = java.math.BigInteger.valueOf(0);
            }
            java.math.BigInteger diff_1 = da_3.subtract(db_3);
            res_3 = _p(diff_1) + res_3;
            i_1 = i_1.subtract(java.math.BigInteger.valueOf(1));
            j_3 = j_3.subtract(java.math.BigInteger.valueOf(1));
        }
        java.math.BigInteger k_1 = java.math.BigInteger.valueOf(0);
        while (k_1.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(res_3)))) < 0 && (String.valueOf(_substr(res_3, (int)(((java.math.BigInteger)(k_1)).longValue()), (int)(((java.math.BigInteger)(k_1.add(java.math.BigInteger.valueOf(1)))).longValue()))).equals(String.valueOf("0")))) {
            k_1 = k_1.add(java.math.BigInteger.valueOf(1));
        }
        if (k_1.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(res_3)))) == 0) {
            return "0";
        }
        return _substr(res_3, (int)(((java.math.BigInteger)(k_1)).longValue()), (int)((long)(_runeLen(res_3))));
    }

    static String mul_digit(String a, java.math.BigInteger d) {
        if (d.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return "0";
        }
        java.math.BigInteger i_3 = new java.math.BigInteger(String.valueOf(_runeLen(a))).subtract(java.math.BigInteger.valueOf(1));
        java.math.BigInteger carry_3 = java.math.BigInteger.valueOf(0);
        String res_5 = "";
        while (i_3.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
            java.math.BigInteger prod_1 = new java.math.BigInteger(String.valueOf(((Number)(((java.math.BigInteger)(digitMap).get(_substr(a, (int)(((java.math.BigInteger)(i_3)).longValue()), (int)(((java.math.BigInteger)(i_3.add(java.math.BigInteger.valueOf(1)))).longValue())))))).intValue())).multiply(d).add(carry_3);
            res_5 = _p(prod_1.remainder(java.math.BigInteger.valueOf(10))) + res_5;
            carry_3 = prod_1.divide(java.math.BigInteger.valueOf(10));
            i_3 = i_3.subtract(java.math.BigInteger.valueOf(1));
        }
        if (carry_3.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            res_5 = _p(carry_3) + res_5;
        }
        java.math.BigInteger k_3 = java.math.BigInteger.valueOf(0);
        while (k_3.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(res_5)))) < 0 && (String.valueOf(_substr(res_5, (int)(((java.math.BigInteger)(k_3)).longValue()), (int)(((java.math.BigInteger)(k_3.add(java.math.BigInteger.valueOf(1)))).longValue()))).equals(String.valueOf("0")))) {
            k_3 = k_3.add(java.math.BigInteger.valueOf(1));
        }
        if (k_3.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(res_5)))) == 0) {
            return "0";
        }
        return _substr(res_5, (int)(((java.math.BigInteger)(k_3)).longValue()), (int)((long)(_runeLen(res_5))));
    }

    static java.util.Map<String,Object> mul_str(String a, String b) {
        String result = "0";
        java.math.BigInteger shift_1 = java.math.BigInteger.valueOf(0);
        Object[] parts_1 = new Object[]{};
        java.math.BigInteger i_5 = new java.math.BigInteger(String.valueOf(_runeLen(b))).subtract(java.math.BigInteger.valueOf(1));
        while (i_5.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
            java.math.BigInteger d_1 = new java.math.BigInteger(String.valueOf(((Number)(((java.math.BigInteger)(digitMap).get(_substr(b, (int)(((java.math.BigInteger)(i_5)).longValue()), (int)(((java.math.BigInteger)(i_5.add(java.math.BigInteger.valueOf(1)))).longValue())))))).intValue()));
            String part_1 = String.valueOf(mul_digit(a, d_1));
            parts_1 = java.util.stream.Stream.concat(java.util.Arrays.stream(parts_1), java.util.stream.Stream.of(new java.util.LinkedHashMap<String, Object>(java.util.Map.of("val", (Object)(part_1), "shift", (Object)(shift_1))))).toArray(java.util.Map[]::new);
            String shifted_1 = part_1;
            for (java.math.BigInteger __3 = java.math.BigInteger.valueOf(0); __3.compareTo(shift_1) < 0; __3 = __3.add(java.math.BigInteger.ONE)) {
                shifted_1 = shifted_1 + "0";
            }
            result = String.valueOf(add_str(result, shifted_1));
            shift_1 = shift_1.add(java.math.BigInteger.valueOf(1));
            i_5 = i_5.subtract(java.math.BigInteger.valueOf(1));
        }
        return ((java.util.Map<String,Object>)(new java.util.LinkedHashMap<String, Object>(java.util.Map.of("res", result, "parts", parts_1))));
    }

    static String pad_left(String s, java.math.BigInteger total) {
        String r_1 = "";
        for (java.math.BigInteger __5 = java.math.BigInteger.valueOf(0); __5.compareTo((total.subtract(new java.math.BigInteger(String.valueOf(_runeLen(s)))))) < 0; __5 = __5.add(java.math.BigInteger.ONE)) {
            r_1 = r_1 + " ";
        }
        return r_1 + s;
    }

    static void main() {
        String tStr = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
        if ((String.valueOf(tStr).equals(String.valueOf("")))) {
            return;
        }
        java.math.BigInteger t_1 = new java.math.BigInteger(String.valueOf(Integer.parseInt(tStr)));
        for (java.math.BigInteger __8 = java.math.BigInteger.valueOf(0); __8.compareTo(t_1) < 0; __8 = __8.add(java.math.BigInteger.ONE)) {
            String line_2 = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            if ((String.valueOf(line_2).equals(String.valueOf("")))) {
                continue;
            }
            java.math.BigInteger idx_1 = java.math.BigInteger.valueOf(0);
            while (idx_1.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(line_2)))) < 0) {
                String ch_1 = _substr(line_2, (int)(((java.math.BigInteger)(idx_1)).longValue()), (int)(((java.math.BigInteger)(idx_1.add(java.math.BigInteger.valueOf(1)))).longValue()));
                if ((String.valueOf(ch_1).equals(String.valueOf("+"))) || (String.valueOf(ch_1).equals(String.valueOf("-"))) || (String.valueOf(ch_1).equals(String.valueOf("*")))) {
                    break;
                }
                idx_1 = idx_1.add(java.math.BigInteger.valueOf(1));
            }
            String a_1 = _substr(line_2, (int)(0L), (int)(((java.math.BigInteger)(idx_1)).longValue()));
            String op_1 = _substr(line_2, (int)(((java.math.BigInteger)(idx_1)).longValue()), (int)(((java.math.BigInteger)(idx_1.add(java.math.BigInteger.valueOf(1)))).longValue()));
            String b_1 = _substr(line_2, (int)(((java.math.BigInteger)(idx_1.add(java.math.BigInteger.valueOf(1)))).longValue()), (int)((long)(_runeLen(line_2))));
            String res_7 = "";
            Object[] parts_3 = new Object[]{};
            if ((String.valueOf(op_1).equals(String.valueOf("+")))) {
                res_7 = String.valueOf(add_str(a_1, b_1));
            } else             if ((String.valueOf(op_1).equals(String.valueOf("-")))) {
                res_7 = String.valueOf(sub_str(a_1, b_1));
            } else {
                java.util.Map<String,Object> r_3 = mul_str(a_1, b_1);
                res_7 = String.valueOf(((String)(((Object)(r_3).get("res")))));
                parts_3 = (Object[])(((Object[])(r_3).get("parts")));
            }
            java.math.BigInteger width_1 = new java.math.BigInteger(String.valueOf(_runeLen(a_1)));
            java.math.BigInteger secondLen_1 = new java.math.BigInteger(String.valueOf(_runeLen(b_1))).add(java.math.BigInteger.valueOf(1));
            if (secondLen_1.compareTo(width_1) > 0) {
                width_1 = secondLen_1;
            }
            if (new java.math.BigInteger(String.valueOf(_runeLen(res_7))).compareTo(width_1) > 0) {
                width_1 = new java.math.BigInteger(String.valueOf(_runeLen(res_7)));
            }
            for (Object p : parts_3) {
                java.math.BigInteger l_1 = new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(String.valueOf(((String)(((Object)(((java.util.Map)p)).get("val"))))).length())).add(new java.math.BigInteger(String.valueOf((((Number)(((Object)(((java.util.Map)p)).get("shift")))).intValue()))))));
                if (l_1.compareTo(width_1) > 0) {
                    width_1 = l_1;
                }
            }
            System.out.println(pad_left(a_1, width_1));
            System.out.println(pad_left(op_1 + b_1, width_1));
            java.math.BigInteger dash1_1 = java.math.BigInteger.valueOf(0);
            if ((String.valueOf(op_1).equals(String.valueOf("*")))) {
                if (new java.math.BigInteger(String.valueOf(parts_3.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                    dash1_1 = new java.math.BigInteger(String.valueOf(_runeLen(b_1))).add(java.math.BigInteger.valueOf(1));
                    String firstPart_1 = String.valueOf(((String)(((Object)(((java.util.Map)parts_3[_idx((parts_3).length, 0L)])).get("val")))));
                    if (new java.math.BigInteger(String.valueOf(_runeLen(firstPart_1))).compareTo(dash1_1) > 0) {
                        dash1_1 = new java.math.BigInteger(String.valueOf(_runeLen(firstPart_1)));
                    }
                } else {
                    dash1_1 = new java.math.BigInteger(String.valueOf(_runeLen(b_1))).add(java.math.BigInteger.valueOf(1));
                    if (new java.math.BigInteger(String.valueOf(_runeLen(res_7))).compareTo(dash1_1) > 0) {
                        dash1_1 = new java.math.BigInteger(String.valueOf(_runeLen(res_7)));
                    }
                }
            } else {
                dash1_1 = new java.math.BigInteger(String.valueOf(_runeLen(b_1))).add(java.math.BigInteger.valueOf(1));
                if (new java.math.BigInteger(String.valueOf(_runeLen(res_7))).compareTo(dash1_1) > 0) {
                    dash1_1 = new java.math.BigInteger(String.valueOf(_runeLen(res_7)));
                }
            }
            System.out.println(pad_left((String)(_repeat("-", ((java.math.BigInteger)(dash1_1)).longValue())), width_1));
            if ((String.valueOf(op_1).equals(String.valueOf("*"))) && new java.math.BigInteger(String.valueOf(_runeLen(b_1))).compareTo(java.math.BigInteger.valueOf(1)) > 0) {
                for (Object p : parts_3) {
                    String val_1 = String.valueOf(((String)(((Object)(((java.util.Map)p)).get("val")))));
                    java.math.BigInteger shift_3 = new java.math.BigInteger(String.valueOf(((Number)(((Object)(((java.util.Map)p)).get("shift")))).intValue()));
                    java.math.BigInteger spaces_1 = width_1.subtract(shift_3).subtract(new java.math.BigInteger(String.valueOf(_runeLen(val_1))));
                    String line_3 = "";
                    for (java.math.BigInteger __9 = java.math.BigInteger.valueOf(0); __9.compareTo(spaces_1) < 0; __9 = __9.add(java.math.BigInteger.ONE)) {
                        line_3 = line_3 + " ";
                    }
                    line_3 = line_3 + val_1;
                    System.out.println(line_3);
                }
                System.out.println(pad_left((String)(_repeat("-", (long)(_runeLen(res_7)))), width_1));
            }
            System.out.println(pad_left(res_7, width_1));
            System.out.println("");
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            digitMap = ((java.util.Map<String,java.math.BigInteger>)(new java.util.LinkedHashMap<String, java.math.BigInteger>(java.util.Map.of("0", java.math.BigInteger.valueOf(0), "1", java.math.BigInteger.valueOf(1), "2", java.math.BigInteger.valueOf(2), "3", java.math.BigInteger.valueOf(3), "4", java.math.BigInteger.valueOf(4), "5", java.math.BigInteger.valueOf(5), "6", java.math.BigInteger.valueOf(6), "7", java.math.BigInteger.valueOf(7), "8", java.math.BigInteger.valueOf(8), "9", java.math.BigInteger.valueOf(9)))));
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

    static String _repeat(String s, long n) {
        StringBuilder sb = new StringBuilder();
        for (long i = 0; i < n; i++) sb.append(s);
        return sb.toString();
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

    static Object _getm(Object m, String k) {
        if (!(m instanceof java.util.Map<?,?>)) return null;
        return ((java.util.Map<?,?>)m).get(k);
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
