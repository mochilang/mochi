public class Main {
    static int[] primes;

    static int[] primesUpTo(int n) {
        boolean[] sieve = new boolean[]{};
        int i = 0;
        while (i <= n) {
            sieve = appendBool(sieve, true);
            i = i + 1;
        }
        int p = 2;
        while (p * p <= n) {
            if (((Boolean)(sieve[p]))) {
                int m = p * p;
                while (m <= n) {
sieve[m] = false;
                    m = m + p;
                }
            }
            p = p + 1;
        }
        int[] res = new int[]{};
        int x = 2;
        while (x <= n) {
            if (((Boolean)(sieve[x]))) {
                res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(x)).toArray();
            }
            x = x + 1;
        }
        return res;
    }

    static int[] sortInts(int[] xs) {
        int[] res_1 = new int[]{};
        int[] tmp = xs;
        while (tmp.length > 0) {
            int min = tmp[0];
            int idx = 0;
            int i_1 = 1;
            while (i_1 < tmp.length) {
                if (tmp[i_1] < min) {
                    min = tmp[i_1];
                    idx = i_1;
                }
                i_1 = i_1 + 1;
            }
            res_1 = java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(min)).toArray();
            int[] out = new int[]{};
            int j = 0;
            while (j < tmp.length) {
                if (j != idx) {
                    out = java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(tmp[j])).toArray();
                }
                j = j + 1;
            }
            tmp = out;
        }
        return res_1;
    }

    static String commatize(int n) {
        String s = _p(n);
        int i_2 = _runeLen(s) - 3;
        while (i_2 >= 1) {
            s = s.substring(0, i_2) + "," + s.substring(i_2, _runeLen(s));
            i_2 = i_2 - 3;
        }
        return s;
    }

    static java.util.Map<String,Object> getBrilliant(int digits, int limit, boolean countOnly) {
        int[] brilliant = new int[]{};
        int count = 0;
        int pow = 1;
        int next = (int)999999999999999L;
        int k = 1;
        while (k <= digits) {
            int[] s_1 = new int[]{};
            for (int p : primes) {
                if (p >= pow * 10) {
                    break;
                }
                if (p > pow) {
                    s_1 = java.util.stream.IntStream.concat(java.util.Arrays.stream(s_1), java.util.stream.IntStream.of(p)).toArray();
                }
            }
            int i_3 = 0;
            while (i_3 < s_1.length) {
                int j_1 = i_3;
                while (j_1 < s_1.length) {
                    int prod = s_1[i_3] * s_1[j_1];
                    if (prod < limit) {
                        if (((Boolean)(countOnly))) {
                            count = count + 1;
                        } else {
                            brilliant = java.util.stream.IntStream.concat(java.util.Arrays.stream(brilliant), java.util.stream.IntStream.of(prod)).toArray();
                        }
                    } else {
                        if (prod < next) {
                            next = prod;
                        }
                        break;
                    }
                    j_1 = j_1 + 1;
                }
                i_3 = i_3 + 1;
            }
            pow = pow * 10;
            k = k + 1;
        }
        if (((Boolean)(countOnly))) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("bc", count), java.util.Map.entry("next", next)));
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("bc", brilliant), java.util.Map.entry("next", next)));
    }

    static void main() {
        System.out.println("First 100 brilliant numbers:");
        java.util.Map<String,Object> r = getBrilliant(2, 10000, false);
        int[] br = sortInts((int[])(((int[]) (r.get("bc")))));
        br = java.util.Arrays.copyOfRange(br, 0, 100);
        int i_4 = 0;
        while (i_4 < br.length) {
            System.out.println((String)(_padStart(_p(_geti(br, i_4)), 4, " ")) + " " + " " + String.valueOf(false ? "True" : "False"));
            if (Math.floorMod((i_4 + 1), 10) == 0) {
                System.out.println("" + " " + String.valueOf(true ? "True" : "False"));
            }
            i_4 = i_4 + 1;
        }
        System.out.println("" + " " + String.valueOf(true ? "True" : "False"));
        int k_1 = 1;
        while (k_1 <= 13) {
            Object limit = pow(10, k_1);
            java.util.Map<String,Object> r2 = getBrilliant(k_1, ((Number)(limit)).intValue(), true);
            int[] total = (int[])(((int[]) (r2.get("bc"))));
            int next_1 = (int)(((int) (r2.get("next"))));
            String climit = String.valueOf(commatize(((Number)(limit)).intValue()));
            String ctotal = String.valueOf(commatize(total + 1));
            String cnext = String.valueOf(commatize(next_1));
            System.out.println("First >= " + (String)(_padStart(climit, 18, " ")) + " is " + (String)(_padStart(ctotal, 14, " ")) + " in the series: " + (String)(_padStart(cnext, 18, " ")));
            k_1 = k_1 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            primes = primesUpTo(3200000);
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _padStart(String s, int width, String pad) {
        String out = s;
        while (out.length() < width) { out = pad + out; }
        return out;
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
