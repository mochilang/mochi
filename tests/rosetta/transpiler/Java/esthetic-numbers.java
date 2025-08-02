public class Main {
    static String digits;
    static int[] esths;

    static String toBase(int n, int b) {
        if (n == 0) {
            return "0";
        }
        int v = n;
        String out = "";
        while (v > 0) {
            int d = Math.floorMod(v, b);
            out = digits.substring(d, d + 1) + out;
            v = v / b;
        }
        return out;
    }

    static int uabs(int a, int b) {
        if (a > b) {
            return a - b;
        }
        return b - a;
    }

    static boolean isEsthetic(int n, int b) {
        if (n == 0) {
            return false;
        }
        int i = Math.floorMod(n, b);
        n = n / b;
        while (n > 0) {
            int j = Math.floorMod(n, b);
            if (uabs(i, j) != 1) {
                return false;
            }
            n = n / b;
            i = j;
        }
        return true;
    }

    static void dfs(int n, int m, int i) {
        if (i >= n && i <= m) {
            esths = java.util.stream.IntStream.concat(java.util.Arrays.stream(esths), java.util.stream.IntStream.of(i)).toArray();
        }
        if (i == 0 || i > m) {
            return;
        }
        int d_1 = Math.floorMod(i, 10);
        int i1 = i * 10 + d_1 - 1;
        int i2 = i1 + 2;
        if (d_1 == 0) {
            dfs(n, m, i2);
        } else         if (d_1 == 9) {
            dfs(n, m, i1);
        } else {
            dfs(n, m, i1);
            dfs(n, m, i2);
        }
    }

    static String commatize(int n) {
        String s = (String)(_p(n));
        int i_1 = _runeLen(s) - 3;
        while (i_1 >= 1) {
            s = s.substring(0, i_1) + "," + s.substring(i_1, _runeLen(s));
            i_1 = i_1 - 3;
        }
        return s;
    }

    static void listEsths(int n, int n2, int m, int m2, int perLine, boolean showAll) {
        esths = new int[]{};
        int i_2 = 0;
        while (i_2 < 10) {
            dfs(n2, m2, i_2);
            i_2 = i_2 + 1;
        }
        int le = esths.length;
        System.out.println("Base 10: " + String.valueOf(commatize(le)) + " esthetic numbers between " + String.valueOf(commatize(n)) + " and " + String.valueOf(commatize(m)) + ":");
        if (showAll) {
            int c = 0;
            String line = "";
            for (int v : esths) {
                if (_runeLen(line) > 0) {
                    line = line + " ";
                }
                line = line + (String)(_p(v));
                c = c + 1;
                if (Math.floorMod(c, perLine) == 0) {
                    System.out.println(line);
                    line = "";
                }
            }
            if (_runeLen(line) > 0) {
                System.out.println(line);
            }
        } else {
            String line_1 = "";
            int idx = 0;
            while (idx < perLine) {
                if (_runeLen(line_1) > 0) {
                    line_1 = line_1 + " ";
                }
                line_1 = line_1 + (String)(_p(_geti(esths, idx)));
                idx = idx + 1;
            }
            System.out.println(line_1);
            System.out.println("............");
            line_1 = "";
            idx = le - perLine;
            while (idx < le) {
                if (_runeLen(line_1) > 0) {
                    line_1 = line_1 + " ";
                }
                line_1 = line_1 + (String)(_p(_geti(esths, idx)));
                idx = idx + 1;
            }
            System.out.println(line_1);
        }
        System.out.println("");
    }

    static void main() {
        int b = 2;
        while (b <= 16) {
            int start = 4 * b;
            int stop = 6 * b;
            System.out.println("Base " + (String)(_p(b)) + ": " + (String)(_p(start)) + "th to " + (String)(_p(stop)) + "th esthetic numbers:");
            int n = 1;
            int c_1 = 0;
            String line_2 = "";
            while (c_1 < stop) {
                if (isEsthetic(n, b)) {
                    c_1 = c_1 + 1;
                    if (c_1 >= start) {
                        if (_runeLen(line_2) > 0) {
                            line_2 = line_2 + " ";
                        }
                        line_2 = line_2 + String.valueOf(toBase(n, b));
                    }
                }
                n = n + 1;
            }
            System.out.println(line_2);
            System.out.println("");
            b = b + 1;
        }
        listEsths(1000, 1010, 9999, 9898, 16, true);
        listEsths(100000000, 101010101, 130000000, 123456789, 9, true);
        listEsths((int)100000000000L, (int)101010101010L, (int)130000000000L, (int)123456789898L, 7, false);
        listEsths((int)100000000000000L, (int)101010101010101L, (int)130000000000000L, (int)123456789898989L, 5, false);
        listEsths((int)100000000000000000L, (int)101010101010101010L, (int)130000000000000000L, (int)123456789898989898L, 4, false);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            digits = "0123456789abcdef";
            esths = new int[]{};
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
