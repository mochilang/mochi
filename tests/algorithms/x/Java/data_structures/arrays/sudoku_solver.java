public class Main {
    static String puzzle = "003020600900305001001806400008102900700000008006708200002609500800203009005010300";
    static java.math.BigInteger[][] grid_1;

    static java.math.BigInteger[][] string_to_grid(String s) {
        java.math.BigInteger[][] grid = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(java.math.BigInteger.valueOf(9)) < 0) {
            java.math.BigInteger[] row_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(java.math.BigInteger.valueOf(9)) < 0) {
                String ch_1 = _substr(s, (int)(((java.math.BigInteger)(i_1.multiply(java.math.BigInteger.valueOf(9)).add(j_1))).longValue()), (int)(((java.math.BigInteger)(i_1.multiply(java.math.BigInteger.valueOf(9)).add(j_1).add(java.math.BigInteger.valueOf(1)))).longValue()));
                java.math.BigInteger val_1 = java.math.BigInteger.valueOf(0);
                if (!(ch_1.equals("0")) && !(ch_1.equals("."))) {
                    val_1 = new java.math.BigInteger(String.valueOf(Integer.parseInt(ch_1)));
                }
                row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of(val_1)).toArray(java.math.BigInteger[]::new)));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            grid = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(grid), java.util.stream.Stream.of(new java.math.BigInteger[][]{row_1})).toArray(java.math.BigInteger[][]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return grid;
    }

    static void print_grid(java.math.BigInteger[][] grid) {
        for (int r = 0; r < 9; r++) {
            String line = "";
            for (int c = 0; c < 9; c++) {
                line = line + _p(_geto(grid[(int)((long)(r))], ((Number)(c)).intValue()));
                if (new java.math.BigInteger(String.valueOf(c)).compareTo(java.math.BigInteger.valueOf(8)) < 0) {
                    line = line + " ";
                }
            }
            System.out.println(line);
        }
    }

    static boolean is_safe(java.math.BigInteger[][] grid, java.math.BigInteger row, java.math.BigInteger column, java.math.BigInteger n) {
        for (int i = 0; i < 9; i++) {
            if (grid[(int)(((java.math.BigInteger)(row)).longValue())][(int)((long)(i))].compareTo(n) == 0 || grid[(int)((long)(i))][(int)(((java.math.BigInteger)(column)).longValue())].compareTo(n) == 0) {
                return false;
            }
        }
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (grid[(int)(((java.math.BigInteger)((row.subtract(row.remainder(java.math.BigInteger.valueOf(3)))).add(new java.math.BigInteger(String.valueOf(i))))).longValue())][(int)(((java.math.BigInteger)((column.subtract(column.remainder(java.math.BigInteger.valueOf(3)))).add(new java.math.BigInteger(String.valueOf(j))))).longValue())].compareTo(n) == 0) {
                    return false;
                }
            }
        }
        return true;
    }

    static java.math.BigInteger[] find_empty(java.math.BigInteger[][] grid) {
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                if (grid[(int)((long)(i))][(int)((long)(j))].compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                    return new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(i)), new java.math.BigInteger(String.valueOf(j))};
                }
            }
        }
        return new java.math.BigInteger[]{};
    }

    static boolean solve(java.math.BigInteger[][] grid) {
        java.math.BigInteger[] loc = ((java.math.BigInteger[])(find_empty(((java.math.BigInteger[][])(grid)))));
        if (new java.math.BigInteger(String.valueOf(loc.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return true;
        }
        java.math.BigInteger row_3 = new java.math.BigInteger(String.valueOf(loc[(int)(0L)]));
        java.math.BigInteger column_1 = new java.math.BigInteger(String.valueOf(loc[(int)(1L)]));
        for (int digit = 1; digit < 10; digit++) {
            if (is_safe(((java.math.BigInteger[][])(grid)), new java.math.BigInteger(String.valueOf(row_3)), new java.math.BigInteger(String.valueOf(column_1)), new java.math.BigInteger(String.valueOf(digit)))) {
grid[(int)(((java.math.BigInteger)(row_3)).longValue())][(int)(((java.math.BigInteger)(column_1)).longValue())] = new java.math.BigInteger(String.valueOf(digit));
                if (solve(((java.math.BigInteger[][])(grid)))) {
                    return true;
                }
grid[(int)(((java.math.BigInteger)(row_3)).longValue())][(int)(((java.math.BigInteger)(column_1)).longValue())] = java.math.BigInteger.valueOf(0);
            }
        }
        return false;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            grid_1 = ((java.math.BigInteger[][])(string_to_grid(puzzle)));
            System.out.println("Original grid:");
            print_grid(((java.math.BigInteger[][])(grid_1)));
            if (solve(((java.math.BigInteger[][])(grid_1)))) {
                System.out.println("\nSolved grid:");
                print_grid(((java.math.BigInteger[][])(grid_1)));
            } else {
                System.out.println("\nNo solution found");
            }
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
