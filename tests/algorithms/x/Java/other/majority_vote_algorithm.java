public class Main {

    static java.math.BigInteger index_of(java.math.BigInteger[] xs, java.math.BigInteger x) {
        java.math.BigInteger i = java.math.BigInteger.valueOf(0);
        while (i.compareTo(new java.math.BigInteger(String.valueOf(xs.length))) < 0) {
            if (xs[_idx((xs).length, ((java.math.BigInteger)(i)).longValue())].compareTo(x) == 0) {
                return i;
            }
            i = i.add(java.math.BigInteger.valueOf(1));
        }
        return (java.math.BigInteger.valueOf(1)).negate();
    }

    static java.math.BigInteger[] majority_vote(java.math.BigInteger[] votes, java.math.BigInteger votes_needed_to_win) {
        if (votes_needed_to_win.compareTo(java.math.BigInteger.valueOf(2)) < 0) {
            return ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        }
        java.math.BigInteger[] candidates_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger[] counts_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_2 = java.math.BigInteger.valueOf(0);
        while (i_2.compareTo(new java.math.BigInteger(String.valueOf(votes.length))) < 0) {
            java.math.BigInteger v_1 = votes[_idx((votes).length, ((java.math.BigInteger)(i_2)).longValue())];
            java.math.BigInteger idx_1 = index_of(((java.math.BigInteger[])(candidates_1)), v_1);
            if (idx_1.compareTo((java.math.BigInteger.valueOf(1)).negate()) != 0) {
counts_1[(int)(((java.math.BigInteger)(idx_1)).longValue())] = counts_1[_idx((counts_1).length, ((java.math.BigInteger)(idx_1)).longValue())].add(java.math.BigInteger.valueOf(1));
            } else             if (new java.math.BigInteger(String.valueOf(candidates_1.length)).compareTo(votes_needed_to_win.subtract(java.math.BigInteger.valueOf(1))) < 0) {
                candidates_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(candidates_1), java.util.stream.Stream.of(v_1)).toArray(java.math.BigInteger[]::new)));
                counts_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(counts_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(1))).toArray(java.math.BigInteger[]::new)));
            } else {
                java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
                while (j_1.compareTo(new java.math.BigInteger(String.valueOf(counts_1.length))) < 0) {
counts_1[(int)(((java.math.BigInteger)(j_1)).longValue())] = counts_1[_idx((counts_1).length, ((java.math.BigInteger)(j_1)).longValue())].subtract(java.math.BigInteger.valueOf(1));
                    j_1 = j_1.add(java.math.BigInteger.valueOf(1));
                }
                java.math.BigInteger[] new_candidates_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
                java.math.BigInteger[] new_counts_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
                j_1 = java.math.BigInteger.valueOf(0);
                while (j_1.compareTo(new java.math.BigInteger(String.valueOf(candidates_1.length))) < 0) {
                    if (counts_1[_idx((counts_1).length, ((java.math.BigInteger)(j_1)).longValue())].compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                        new_candidates_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_candidates_1), java.util.stream.Stream.of(candidates_1[_idx((candidates_1).length, ((java.math.BigInteger)(j_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
                        new_counts_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_counts_1), java.util.stream.Stream.of(counts_1[_idx((counts_1).length, ((java.math.BigInteger)(j_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
                    }
                    j_1 = j_1.add(java.math.BigInteger.valueOf(1));
                }
                candidates_1 = ((java.math.BigInteger[])(new_candidates_1));
                counts_1 = ((java.math.BigInteger[])(new_counts_1));
            }
            i_2 = i_2.add(java.math.BigInteger.valueOf(1));
        }
        java.math.BigInteger[] final_counts_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger j_3 = java.math.BigInteger.valueOf(0);
        while (j_3.compareTo(new java.math.BigInteger(String.valueOf(candidates_1.length))) < 0) {
            final_counts_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(final_counts_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
            j_3 = j_3.add(java.math.BigInteger.valueOf(1));
        }
        i_2 = java.math.BigInteger.valueOf(0);
        while (i_2.compareTo(new java.math.BigInteger(String.valueOf(votes.length))) < 0) {
            java.math.BigInteger v_3 = votes[_idx((votes).length, ((java.math.BigInteger)(i_2)).longValue())];
            java.math.BigInteger idx_3 = index_of(((java.math.BigInteger[])(candidates_1)), v_3);
            if (idx_3.compareTo((java.math.BigInteger.valueOf(1)).negate()) != 0) {
final_counts_1[(int)(((java.math.BigInteger)(idx_3)).longValue())] = final_counts_1[_idx((final_counts_1).length, ((java.math.BigInteger)(idx_3)).longValue())].add(java.math.BigInteger.valueOf(1));
            }
            i_2 = i_2.add(java.math.BigInteger.valueOf(1));
        }
        java.math.BigInteger[] result_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        j_3 = java.math.BigInteger.valueOf(0);
        while (j_3.compareTo(new java.math.BigInteger(String.valueOf(candidates_1.length))) < 0) {
            if (final_counts_1[_idx((final_counts_1).length, ((java.math.BigInteger)(j_3)).longValue())].multiply(votes_needed_to_win).compareTo(new java.math.BigInteger(String.valueOf(votes.length))) > 0) {
                result_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(candidates_1[_idx((candidates_1).length, ((java.math.BigInteger)(j_3)).longValue())])).toArray(java.math.BigInteger[]::new)));
            }
            j_3 = j_3.add(java.math.BigInteger.valueOf(1));
        }
        return ((java.math.BigInteger[])(result_1));
    }

    static void main() {
        java.math.BigInteger[] votes = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(2)}));
        System.out.println(_p(majority_vote(((java.math.BigInteger[])(votes)), java.math.BigInteger.valueOf(3))));
        System.out.println(_p(majority_vote(((java.math.BigInteger[])(votes)), java.math.BigInteger.valueOf(2))));
        System.out.println(_p(majority_vote(((java.math.BigInteger[])(votes)), java.math.BigInteger.valueOf(4))));
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
