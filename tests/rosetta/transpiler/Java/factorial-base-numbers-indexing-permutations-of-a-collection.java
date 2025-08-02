public class Main {
    static int seed;

    static String[] split(String s, String sep) {
        String[] parts = new String[]{};
        String cur = "";
        int i = 0;
        while (i < _runeLen(s)) {
            if (_runeLen(sep) > 0 && i + _runeLen(sep) <= _runeLen(s) && (_substr(s, i, i + _runeLen(sep)).equals(sep))) {
                parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                cur = "";
                i = i + _runeLen(sep);
            } else {
                cur = cur + _substr(s, i, i + 1);
                i = i + 1;
            }
        }
        parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        return parts;
    }

    static int parseIntStr(String str) {
        int i_1 = 0;
        boolean neg = false;
        if (_runeLen(str) > 0 && (str.substring(0, 1).equals("-"))) {
            neg = true;
            i_1 = 1;
        }
        int n = 0;
        java.util.Map<String,Integer> digits = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1), java.util.Map.entry("2", 2), java.util.Map.entry("3", 3), java.util.Map.entry("4", 4), java.util.Map.entry("5", 5), java.util.Map.entry("6", 6), java.util.Map.entry("7", 7), java.util.Map.entry("8", 8), java.util.Map.entry("9", 9)))));
        while (i_1 < _runeLen(str)) {
            n = n * 10 + (int)(((int)(digits).get(str.substring(i_1, i_1 + 1))));
            i_1 = i_1 + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }

    static String joinInts(int[] nums, String sep) {
        String s = "";
        int i_2 = 0;
        while (i_2 < nums.length) {
            if (i_2 > 0) {
                s = s + sep;
            }
            s = s + (String)(_p(_geti(nums, i_2)));
            i_2 = i_2 + 1;
        }
        return s;
    }

    static int[] undot(String s) {
        String[] parts_1 = s.split(".");
        int[] nums = new int[]{};
        for (String p : parts_1) {
            nums = java.util.stream.IntStream.concat(java.util.Arrays.stream(nums), java.util.stream.IntStream.of(Integer.parseInt(p))).toArray();
        }
        return nums;
    }

    static int factorial(int n) {
        int f = 1;
        int i_3 = 2;
        while (i_3 <= n) {
            f = f * i_3;
            i_3 = i_3 + 1;
        }
        return f;
    }

    static Object[] genFactBaseNums(int size, boolean countOnly) {
        int[][] results = new int[][]{};
        int count = 0;
        int n_1 = 0;
        while (true) {
            int radix = 2;
            int[] res = new int[]{};
            if (!(Boolean)countOnly) {
                int z = 0;
                while (z < size) {
                    res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(0)).toArray();
                    z = z + 1;
                }
            }
            int k = n_1;
            while (k > 0) {
                int div = k / radix;
                int rem = Math.floorMod(k, radix);
                if (!(Boolean)countOnly && radix <= size + 1) {
res[size - radix + 1] = rem;
                }
                k = div;
                radix = radix + 1;
            }
            if (radix > size + 2) {
                break;
            }
            count = count + 1;
            if (!(Boolean)countOnly) {
                results = appendObj(results, res);
            }
            n_1 = n_1 + 1;
        }
        return new Object[]{results, count};
    }

    static int[][] mapToPerms(int[][] factNums) {
        int[][] perms = new int[][]{};
        int psize = factNums[0].length + 1;
        int[] start = new int[]{};
        int i_4 = 0;
        while (i_4 < psize) {
            start = java.util.stream.IntStream.concat(java.util.Arrays.stream(start), java.util.stream.IntStream.of(i_4)).toArray();
            i_4 = i_4 + 1;
        }
        for (int[] fn : factNums) {
            int[] perm = new int[]{};
            int j = 0;
            while (j < start.length) {
                perm = java.util.stream.IntStream.concat(java.util.Arrays.stream(perm), java.util.stream.IntStream.of(start[j])).toArray();
                j = j + 1;
            }
            int m = 0;
            while (m < fn.length) {
                int g = fn[m];
                if (g != 0) {
                    int first = m;
                    int last = m + g;
                    int t = 1;
                    while (t <= g) {
                        int temp = perm[first];
                        int x = first + 1;
                        while (x <= last) {
perm[x - 1] = perm[x];
                            x = x + 1;
                        }
perm[last] = temp;
                        t = t + 1;
                    }
                }
                m = m + 1;
            }
            perms = appendObj(perms, perm);
        }
        return perms;
    }

    static int randInt(int n) {
        seed = Math.floorMod((seed * 1664525 + 1013904223), 2147483647);
        return Math.floorMod(seed, n);
    }

    static void main() {
        Object[] g_1 = genFactBaseNums(3, false);
        Object factNums = g_1[0];
        int[][] perms_1 = mapToPerms(factNums);
        int i_5 = 0;
        while (i_5 < String.valueOf(factNums).length()) {
            System.out.println(String.valueOf(joinInts(factNums[i_5], ".")) + " -> " + String.valueOf(joinInts(perms_1[i_5], "")));
            i_5 = i_5 + 1;
        }
        int count2 = factorial(11);
        System.out.println("\nPermutations generated = " + (String)(_p(count2)));
        System.out.println("compared to 11! which  = " + (String)(_p(factorial(11))));
        System.out.println("");
        String[] fbn51s = new String[]{"39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0", "51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1"};
        factNums = new int[][]{undot(fbn51s[0]), undot(fbn51s[1])};
        perms_1 = mapToPerms(factNums);
        String shoe = "A♠K♠Q♠J♠T♠9♠8♠7♠6♠5♠4♠3♠2♠A♥K♥Q♥J♥T♥9♥8♥7♥6♥5♥4♥3♥2♥A♦K♦Q♦J♦T♦9♦8♦7♦6♦5♦4♦3♦2♦A♣K♣Q♣J♣T♣9♣8♣7♣6♣5♣4♣3♣2♣";
        String[] cards = new String[]{};
        i_5 = 0;
        while (i_5 < 52) {
            String card = _substr(shoe, 2 * i_5, 2 * i_5 + 2);
            if ((card.substring(0, 1).equals("T"))) {
                card = "10" + card.substring(1, 2);
            }
            cards = java.util.stream.Stream.concat(java.util.Arrays.stream(cards), java.util.stream.Stream.of(card)).toArray(String[]::new);
            i_5 = i_5 + 1;
        }
        i_5 = 0;
        while (i_5 < fbn51s.length) {
            System.out.println(fbn51s[i_5]);
            int[] perm_1 = perms_1[i_5];
            int j_1 = 0;
            String line = "";
            while (j_1 < perm_1.length) {
                line = line + cards[perm_1[j_1]];
                j_1 = j_1 + 1;
            }
            System.out.println(line + "\n");
            i_5 = i_5 + 1;
        }
        int[] fbn51 = new int[]{};
        i_5 = 0;
        while (i_5 < 51) {
            fbn51 = java.util.stream.IntStream.concat(java.util.Arrays.stream(fbn51), java.util.stream.IntStream.of(randInt(52 - i_5))).toArray();
            i_5 = i_5 + 1;
        }
        System.out.println(joinInts(fbn51, "."));
        perms_1 = mapToPerms(new int[][]{fbn51});
        String line_1 = "";
        i_5 = 0;
        while (i_5 < perms_1[0].length) {
            line_1 = line_1 + cards[perms_1[0][i_5]];
            i_5 = i_5 + 1;
        }
        System.out.println(line_1);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = 1;
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
