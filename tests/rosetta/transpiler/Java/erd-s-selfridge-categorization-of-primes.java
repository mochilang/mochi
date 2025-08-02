public class Main {
    static java.util.Map<Integer,Integer> prevCats;

    static int[] generatePrimes(int n) {
        int[] primes = new int[]{2};
        int cand = 3;
        while (primes.length < n) {
            boolean isP = true;
            int i = 0;
            while (i < primes.length) {
                int p = primes[i];
                if (p * p > cand) {
                    break;
                }
                if (Math.floorMod(cand, p) == 0) {
                    isP = false;
                    break;
                }
                i = i + 1;
            }
            if (isP) {
                primes = java.util.stream.IntStream.concat(java.util.Arrays.stream(primes), java.util.stream.IntStream.of(cand)).toArray();
            }
            cand = cand + 2;
        }
        return primes;
    }

    static int[] primeFactors(int n, int[] primes) {
        int[] factors = new int[]{};
        int num = n;
        int i_1 = 0;
        while (i_1 < primes.length && primes[i_1] * primes[i_1] <= num) {
            int p_1 = primes[i_1];
            while (Math.floorMod(num, p_1) == 0) {
                factors = java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(p_1)).toArray();
                num = num / p_1;
            }
            i_1 = i_1 + 1;
        }
        if (num > 1) {
            factors = java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(num)).toArray();
        }
        return factors;
    }

    static int cat(int p, int[] primes) {
        if (prevCats.containsKey(p)) {
            return ((int)(prevCats).getOrDefault(p, 0));
        }
        int[] pf = primeFactors(p + 1, primes);
        boolean all23 = true;
        for (int f : pf) {
            if (f != 2 && f != 3) {
                all23 = false;
                break;
            }
        }
        if (all23) {
prevCats.put(p, 1);
            return 1;
        }
        if (p > 2) {
            int[] unique = new int[]{};
            int last = -1;
            for (int f : pf) {
                if (f != last) {
                    unique = java.util.stream.IntStream.concat(java.util.Arrays.stream(unique), java.util.stream.IntStream.of(f)).toArray();
                    last = f;
                }
            }
            pf = unique;
        }
        int c = 2;
        while (c <= 11) {
            boolean ok = true;
            for (int f : pf) {
                if (cat(f, primes) >= c) {
                    ok = false;
                    break;
                }
            }
            if (ok) {
prevCats.put(p, c);
                return c;
            }
            c = c + 1;
        }
prevCats.put(p, 12);
        return 12;
    }

    static String padLeft(int n, int width) {
        String s = String.valueOf(n);
        while (_runeLen(s) < width) {
            s = " " + s;
        }
        return s;
    }

    static void main() {
        int[] primes_1 = generatePrimes(1000);
        int[][] es = new int[][]{};
        for (int _v = 0; _v < 12; _v++) {
            es = appendObj(es, new int[]{});
        }
        System.out.println("First 200 primes:\n");
        int idx = 0;
        while (idx < 200) {
            int p_2 = primes_1[idx];
            int c_1 = cat(p_2, primes_1);
es[c_1 - 1] = java.util.stream.IntStream.concat(java.util.Arrays.stream(es[c_1 - 1]), java.util.stream.IntStream.of(p_2)).toArray();
            idx = idx + 1;
        }
        int c_2 = 1;
        while (c_2 <= 6) {
            if (es[c_2 - 1].length > 0) {
                System.out.println("Category " + String.valueOf(c_2) + ":");
                System.out.println(String.valueOf(es[c_2 - 1]));
                System.out.println("");
            }
            c_2 = c_2 + 1;
        }
        System.out.println("First thousand primes:\n");
        while (idx < 1000) {
            int p_3 = primes_1[idx];
            int cv = cat(p_3, primes_1);
es[cv - 1] = java.util.stream.IntStream.concat(java.util.Arrays.stream(es[cv - 1]), java.util.stream.IntStream.of(p_3)).toArray();
            idx = idx + 1;
        }
        c_2 = 1;
        while (c_2 <= 12) {
            int[] e = es[c_2 - 1];
            if (e.length > 0) {
                String line = "Category " + String.valueOf(padLeft(c_2, 2)) + ": First = " + String.valueOf(padLeft(e[0], 7)) + "  Last = " + String.valueOf(padLeft(e[e.length - 1], 8)) + "  Count = " + String.valueOf(padLeft(e.length, 6));
                System.out.println(line);
            }
            c_2 = c_2 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            prevCats = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
