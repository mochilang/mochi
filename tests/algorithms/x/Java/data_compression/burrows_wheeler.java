public class Main {
    static class BWTResult {
        String bwt_string;
        java.math.BigInteger idx_original_string;
        BWTResult(String bwt_string, java.math.BigInteger idx_original_string) {
            this.bwt_string = bwt_string;
            this.idx_original_string = idx_original_string;
        }
        BWTResult() {}
        @Override public String toString() {
            return String.format("{'bwt_string': '%s', 'idx_original_string': %s}", String.valueOf(bwt_string), String.valueOf(idx_original_string));
        }
    }

    static String s = "^BANANA";
    static BWTResult result;

    static String[] all_rotations(String s) {
        java.math.BigInteger n = new java.math.BigInteger(String.valueOf(_runeLen(s)));
        String[] rotations_1 = ((String[])(new String[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) < 0) {
            String rotation_1 = _substr(s, (int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(n)).longValue())) + _substr(s, (int)(0L), (int)(((java.math.BigInteger)(i_1)).longValue()));
            rotations_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(rotations_1), java.util.stream.Stream.of(rotation_1)).toArray(String[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return rotations_1;
    }

    static String[] sort_strings(String[] arr) {
        java.math.BigInteger n_1 = new java.math.BigInteger(String.valueOf(arr.length));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(1);
        while (i_3.compareTo(n_1) < 0) {
            String key_1 = arr[(int)(((java.math.BigInteger)(i_3)).longValue())];
            java.math.BigInteger j_1 = new java.math.BigInteger(String.valueOf(i_3.subtract(java.math.BigInteger.valueOf(1))));
            while (j_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0 && (arr[(int)(((java.math.BigInteger)(j_1)).longValue())].compareTo(key_1) > 0)) {
arr[(int)(((java.math.BigInteger)(j_1.add(java.math.BigInteger.valueOf(1)))).longValue())] = arr[(int)(((java.math.BigInteger)(j_1)).longValue())];
                j_1 = new java.math.BigInteger(String.valueOf(j_1.subtract(java.math.BigInteger.valueOf(1))));
            }
arr[(int)(((java.math.BigInteger)(j_1.add(java.math.BigInteger.valueOf(1)))).longValue())] = key_1;
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return arr;
    }

    static String join_strings(String[] arr) {
        String res = "";
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            res = res + arr[(int)(((java.math.BigInteger)(i_5)).longValue())];
            i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
        }
        return res;
    }

    static BWTResult bwt_transform(String s) {
        if ((s.equals(""))) {
            throw new RuntimeException(String.valueOf("input string must not be empty"));
        }
        String[] rotations_3 = ((String[])(all_rotations(s)));
        rotations_3 = ((String[])(sort_strings(((String[])(rotations_3)))));
        String[] last_col_1 = ((String[])(new String[]{}));
        java.math.BigInteger i_7 = java.math.BigInteger.valueOf(0);
        while (i_7.compareTo(new java.math.BigInteger(String.valueOf(rotations_3.length))) < 0) {
            String word_1 = rotations_3[(int)(((java.math.BigInteger)(i_7)).longValue())];
            last_col_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(last_col_1), java.util.stream.Stream.of(_substr(word_1, (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(_runeLen(word_1))).subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)((long)(_runeLen(word_1)))))).toArray(String[]::new)));
            i_7 = new java.math.BigInteger(String.valueOf(i_7.add(java.math.BigInteger.valueOf(1))));
        }
        String bwt_string_1 = String.valueOf(join_strings(((String[])(last_col_1))));
        java.math.BigInteger idx_1 = new java.math.BigInteger(String.valueOf(index_of(((String[])(rotations_3)), s)));
        return new BWTResult(bwt_string_1, idx_1);
    }

    static java.math.BigInteger index_of(String[] arr, String target) {
        java.math.BigInteger i_8 = java.math.BigInteger.valueOf(0);
        while (i_8.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            if ((arr[(int)(((java.math.BigInteger)(i_8)).longValue())].equals(target))) {
                return i_8;
            }
            i_8 = new java.math.BigInteger(String.valueOf(i_8.add(java.math.BigInteger.valueOf(1))));
        }
        return (java.math.BigInteger.valueOf(1)).negate();
    }

    static String reverse_bwt(String bwt_string, java.math.BigInteger idx_original_string) {
        if ((bwt_string.equals(""))) {
            throw new RuntimeException(String.valueOf("bwt string must not be empty"));
        }
        java.math.BigInteger n_3 = new java.math.BigInteger(String.valueOf(_runeLen(bwt_string)));
        if (idx_original_string.compareTo(java.math.BigInteger.valueOf(0)) < 0 || idx_original_string.compareTo(n_3) >= 0) {
            throw new RuntimeException(String.valueOf("index out of range"));
        }
        String[] ordered_rotations_1 = ((String[])(new String[]{}));
        java.math.BigInteger i_10 = java.math.BigInteger.valueOf(0);
        while (i_10.compareTo(n_3) < 0) {
            ordered_rotations_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ordered_rotations_1), java.util.stream.Stream.of("")).toArray(String[]::new)));
            i_10 = new java.math.BigInteger(String.valueOf(i_10.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger iter_1 = java.math.BigInteger.valueOf(0);
        while (iter_1.compareTo(n_3) < 0) {
            java.math.BigInteger j_3 = java.math.BigInteger.valueOf(0);
            while (j_3.compareTo(n_3) < 0) {
                String ch_1 = _substr(bwt_string, (int)(((java.math.BigInteger)(j_3)).longValue()), (int)(((java.math.BigInteger)(j_3.add(java.math.BigInteger.valueOf(1)))).longValue()));
ordered_rotations_1[(int)(((java.math.BigInteger)(j_3)).longValue())] = ch_1 + ordered_rotations_1[(int)(((java.math.BigInteger)(j_3)).longValue())];
                j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
            }
            ordered_rotations_1 = ((String[])(sort_strings(((String[])(ordered_rotations_1)))));
            iter_1 = new java.math.BigInteger(String.valueOf(iter_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ordered_rotations_1[(int)(((java.math.BigInteger)(idx_original_string)).longValue())];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            result = bwt_transform(s);
            System.out.println(result.bwt_string);
            System.out.println(result.idx_original_string);
            System.out.println(reverse_bwt(result.bwt_string, new java.math.BigInteger(String.valueOf(result.idx_original_string))));
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
}
