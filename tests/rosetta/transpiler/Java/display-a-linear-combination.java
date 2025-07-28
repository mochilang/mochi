public class Main {

    static String padRight(String s, int w) {
        String r = s;
        while (_runeLen(r) < w) {
            r = r + " ";
        }
        return r;
    }

    static String linearCombo(int[] c) {
        String out = "";
        int i = 0;
        while (i < c.length) {
            int n = c[i];
            if (n != 0) {
                String op = "";
                if (n < 0 && _runeLen(out) == 0) {
                    op = "-";
                } else                 if (n < 0) {
                    op = " - ";
                } else                 if (n > 0 && _runeLen(out) == 0) {
                    op = "";
                } else {
                    op = " + ";
                }
                int av = n;
                if (av < 0) {
                    av = -av;
                }
                String coeff = String.valueOf(av) + "*";
                if (av == 1) {
                    coeff = "";
                }
                out = out + op + coeff + "e(" + String.valueOf(i + 1) + ")";
            }
            i = i + 1;
        }
        if (_runeLen(out) == 0) {
            return "0";
        }
        return out;
    }

    static void main() {
        int[][] combos = new int[][]{new int[]{1, 2, 3}, new int[]{0, 1, 2, 3}, new int[]{1, 0, 3, 4}, new int[]{1, 2, 0}, new int[]{0, 0, 0}, new int[]{0}, new int[]{1, 1, 1}, new int[]{-1, -1, -1}, new int[]{-1, -2, 0, -3}, new int[]{-1}};
        int idx = 0;
        while (idx < combos.length) {
            int[] c = combos[idx];
            String t = "[";
            int j = 0;
            while (j < c.length) {
                t = t + String.valueOf(c[j]);
                if (j < c.length - 1) {
                    t = t + ", ";
                }
                j = j + 1;
            }
            t = t + "]";
            String lc = String.valueOf(linearCombo(c));
            System.out.println(String.valueOf(padRight(t, 15)) + "  ->  " + lc);
            idx = idx + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
}
