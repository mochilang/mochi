public class Main {

    static int[] getDivisors(int n) {
        int[] divs = ((int[])(new int[]{1, n}));
        int i = 2;
        while (i * i <= n) {
            if (Math.floorMod(n, i) == 0) {
                int j = ((Number)((n / i))).intValue();
                divs = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(divs), java.util.stream.IntStream.of(i)).toArray()));
                if (i != j) {
                    divs = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(divs), java.util.stream.IntStream.of(j)).toArray()));
                }
            }
            i = i + 1;
        }
        return divs;
    }

    static int sum(int[] xs) {
        int s = 0;
        for (int x : xs) {
            s = s + x;
        }
        return s;
    }

    static boolean isPartSum(int[] divs, int target) {
        boolean[] possible = ((boolean[])(new boolean[]{}));
        int i_1 = 0;
        while (i_1 <= target) {
            possible = ((boolean[])(appendBool(possible, false)));
            i_1 = i_1 + 1;
        }
possible[0] = true;
        for (int v : divs) {
            int s_1 = target;
            while (s_1 >= v) {
                if (((Boolean)(possible[s_1 - v]))) {
possible[s_1] = true;
                }
                s_1 = s_1 - 1;
            }
        }
        return possible[target];
    }

    static boolean isZumkeller(int n) {
        int[] divs_1 = ((int[])(getDivisors(n)));
        int s_2 = sum(((int[])(divs_1)));
        if (Math.floorMod(s_2, 2) == 1) {
            return false;
        }
        if (Math.floorMod(n, 2) == 1) {
            int abundance = s_2 - 2 * n;
            return abundance > 0 && Math.floorMod(abundance, 2) == 0;
        }
        return isPartSum(((int[])(divs_1)), s_2 / 2);
    }

    static String pad(int n, int width) {
        String s_3 = _p(n);
        while (_runeLen(s_3) < width) {
            s_3 = " " + s_3;
        }
        return s_3;
    }

    static void main() {
        System.out.println("The first 220 Zumkeller numbers are:");
        int count = 0;
        String line = "";
        int i_2 = 2;
        while (count < 220) {
            if (((Boolean)(isZumkeller(i_2)))) {
                line = line + String.valueOf(pad(i_2, 3)) + " ";
                count = count + 1;
                if (Math.floorMod(count, 20) == 0) {
                    System.out.println(_substr(line, 0, _runeLen(line) - 1));
                    line = "";
                }
            }
            i_2 = i_2 + 1;
        }
        System.out.println("\nThe first 40 odd Zumkeller numbers are:");
        count = 0;
        line = "";
        i_2 = 3;
        while (count < 40) {
            if (((Boolean)(isZumkeller(i_2)))) {
                line = line + String.valueOf(pad(i_2, 5)) + " ";
                count = count + 1;
                if (Math.floorMod(count, 10) == 0) {
                    System.out.println(_substr(line, 0, _runeLen(line) - 1));
                    line = "";
                }
            }
            i_2 = i_2 + 2;
        }
        System.out.println("\nThe first 40 odd Zumkeller numbers which don't end in 5 are:");
        count = 0;
        line = "";
        i_2 = 3;
        while (count < 40) {
            if (Math.floorMod(i_2, 10) != 5 && ((Boolean)(isZumkeller(i_2)))) {
                line = line + String.valueOf(pad(i_2, 7)) + " ";
                count = count + 1;
                if (Math.floorMod(count, 8) == 0) {
                    System.out.println(_substr(line, 0, _runeLen(line) - 1));
                    line = "";
                }
            }
            i_2 = i_2 + 2;
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
}
