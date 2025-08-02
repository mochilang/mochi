public class Main {

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        if (Math.floorMod(n, 2) == 0) {
            return n == 2;
        }
        if (Math.floorMod(n, 3) == 0) {
            return n == 3;
        }
        int d = 5;
        while (d * d <= n) {
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 2;
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 4;
        }
        return true;
    }

    static int firstPrimeFactor(int n) {
        if (n == 1) {
            return 1;
        }
        if (Math.floorMod(n, 3) == 0) {
            return 3;
        }
        if (Math.floorMod(n, 5) == 0) {
            return 5;
        }
        int[] inc = new int[]{4, 2, 4, 2, 4, 6, 2, 6};
        int k = 7;
        int i = 0;
        while (k * k <= n) {
            if (Math.floorMod(n, k) == 0) {
                return k;
            }
            k = k + inc[i];
            i = Math.floorMod((i + 1), inc.length);
        }
        return n;
    }

    static int indexOf(String s, String ch) {
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            if ((_substr(s, i_1, i_1 + 1).equals(ch))) {
                return i_1;
            }
            i_1 = i_1 + 1;
        }
        return -1;
    }

    static String padLeft(int n, int width) {
        String s = _p(n);
        while (_runeLen(s) < width) {
            s = " " + s;
        }
        return s;
    }

    static String formatFloat(double f, int prec) {
        String s_1 = _p(f);
        int idx = ((Number)(s_1.indexOf("."))).intValue();
        if (idx < 0) {
            return s_1;
        }
        int need = idx + 1 + prec;
        if (_runeLen(s_1) > need) {
            return _substr(s_1, 0, need);
        }
        return s_1;
    }

    static void main() {
        int[] blum = new int[]{};
        int[] counts = new int[]{0, 0, 0, 0};
        int[] digits = new int[]{1, 3, 7, 9};
        int i_2 = 1;
        int bc = 0;
        while (true) {
            int p = firstPrimeFactor(i_2);
            if (Math.floorMod(p, 4) == 3) {
                int q = ((Number)((i_2 / p))).intValue();
                if (q != p && Math.floorMod(q, 4) == 3 && ((Boolean)(isPrime(q)))) {
                    if (bc < 50) {
                        blum = java.util.stream.IntStream.concat(java.util.Arrays.stream(blum), java.util.stream.IntStream.of(i_2)).toArray();
                    }
                    int d_1 = Math.floorMod(i_2, 10);
                    if (d_1 == 1) {
counts[0] = counts[0] + 1;
                    } else                     if (d_1 == 3) {
counts[1] = counts[1] + 1;
                    } else                     if (d_1 == 7) {
counts[2] = counts[2] + 1;
                    } else                     if (d_1 == 9) {
counts[3] = counts[3] + 1;
                    }
                    bc = bc + 1;
                    if (bc == 50) {
                        System.out.println("First 50 Blum integers:");
                        int idx_1 = 0;
                        while (idx_1 < 50) {
                            String line = "";
                            int j = 0;
                            while (j < 10) {
                                line = line + String.valueOf(padLeft(blum[idx_1], 3)) + " ";
                                idx_1 = idx_1 + 1;
                                j = j + 1;
                            }
                            System.out.println(_substr(line, 0, _runeLen(line) - 1));
                        }
                        break;
                    }
                }
            }
            if (Math.floorMod(i_2, 5) == 3) {
                i_2 = i_2 + 4;
            } else {
                i_2 = i_2 + 2;
            }
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
