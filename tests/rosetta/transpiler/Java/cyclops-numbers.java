public class Main {
    static int[] digits(int n) {
        if (n == 0) {
            return new int[]{0};
        }
        int[] rev = new int[]{};
        int x = n;
        while (x > 0) {
            rev = java.util.stream.IntStream.concat(java.util.Arrays.stream(rev), java.util.stream.IntStream.of(Math.floorMod(x, 10))).toArray();
            x = ((Number)((x / 10))).intValue();
        }
        int[] out = new int[]{};
        int i = rev.length - 1;
        while (i >= 0) {
            out = java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(rev[i])).toArray();
            i = i - 1;
        }
        return out;
    }

    static String commatize(int n) {
        String s = String.valueOf(n);
        String out = "";
        int i = s.length();
        while (i > 3) {
            out = "," + s.substring(i - 3, i) + out;
            i = i - 3;
        }
        out = s.substring(0, i) + out;
        return out;
    }

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

    static String[] split(String s, String sep) {
        String[] parts = new String[]{};
        String cur = "";
        int i = 0;
        while (i < s.length()) {
            if (i + sep.length() <= s.length() && (s.substring(i, i + sep.length()).equals(sep))) {
                parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                cur = "";
                i = i + sep.length();
            } else {
                cur = cur + s.substring(i, i + 1);
                i = i + 1;
            }
        }
        parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        return parts;
    }

    static int parseIntStr(String str) {
        int i = 0;
        boolean neg = false;
        if (str.length() > 0 && (str.substring(0, 1).equals("-"))) {
            neg = true;
            i = 1;
        }
        int n = 0;
        java.util.Map<String,Integer> digits = new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1), java.util.Map.entry("2", 2), java.util.Map.entry("3", 3), java.util.Map.entry("4", 4), java.util.Map.entry("5", 5), java.util.Map.entry("6", 6), java.util.Map.entry("7", 7), java.util.Map.entry("8", 8), java.util.Map.entry("9", 9)));
        while (i < str.length()) {
            n = n * 10 + (int)(((int)digits.getOrDefault(str.substring(i, i + 1), 0)));
            i = i + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }

    static String reverseStr(String s) {
        String out = "";
        int i = s.length() - 1;
        while (i >= 0) {
            out = out + s.substring(i, i + 1);
            i = i - 1;
        }
        return out;
    }

    static String pad(String s, int w) {
        String out = s;
        while (out.length() < w) {
            out = " " + out;
        }
        return out;
    }

    static int[] findFirst(int[] list) {
        int i = 0;
        while (i < list.length) {
            if (list[i] > 10000000) {
                return new int[]{list[i], i};
            }
            i = i + 1;
        }
        return new int[]{-1, -1};
    }

    static void main() {
        int[][] ranges = new int[][]{new int[]{0, 0}, new int[]{101, 909}, new int[]{11011, 99099}, new int[]{1110111, 9990999}, new int[]{111101111, 119101111}};
        int[] cyclops = new int[]{};
        for (int[] r : ranges) {
            int start = r[0];
            int end = r[1];
            int numDigits = String.valueOf(start).length();
            int center = numDigits / 2;
            int i = start;
            while (i <= end) {
                int[] ds = digits(i);
                if (ds[center] == 0) {
                    int count = 0;
                    for (int d : ds) {
                        if (d == 0) {
                            count = count + 1;
                        }
                    }
                    if (count == 1) {
                        cyclops = java.util.stream.IntStream.concat(java.util.Arrays.stream(cyclops), java.util.stream.IntStream.of(i)).toArray();
                    }
                }
                i = i + 1;
            }
        }
        System.out.println("The first 50 cyclops numbers are:");
        int idx = 0;
        while (idx < 50) {
            System.out.println(String.valueOf(pad(String.valueOf(commatize(cyclops[idx])), 6)) + " ");
            idx = idx + 1;
            if (Math.floorMod(idx, 10) == 0) {
                System.out.println("\n");
            }
        }
        int[] fi = findFirst(cyclops);
        System.out.println("\nFirst such number > 10 million is " + String.valueOf(commatize(fi[0])) + " at zero-based index " + String.valueOf(commatize(fi[1])));
        int[] primes = new int[]{};
        for (int n : cyclops) {
            if (isPrime(n)) {
                primes = java.util.stream.IntStream.concat(java.util.Arrays.stream(primes), java.util.stream.IntStream.of(n)).toArray();
            }
        }
        System.out.println("\n\nThe first 50 prime cyclops numbers are:");
        idx = 0;
        while (idx < 50) {
            System.out.println(String.valueOf(pad(String.valueOf(commatize(primes[idx])), 6)) + " ");
            idx = idx + 1;
            if (Math.floorMod(idx, 10) == 0) {
                System.out.println("\n");
            }
        }
        int[] fp = findFirst(primes);
        System.out.println("\nFirst such number > 10 million is " + String.valueOf(commatize(fp[0])) + " at zero-based index " + String.valueOf(commatize(fp[1])));
        int[] bpcyclops = new int[]{};
        int[] ppcyclops = new int[]{};
        for (int p : primes) {
            String ps = String.valueOf(p);
            String[] splitp = ps.split("0");
            int noMiddle = Integer.parseInt(splitp[0] + splitp[1]);
            if (isPrime(noMiddle)) {
                bpcyclops = java.util.stream.IntStream.concat(java.util.Arrays.stream(bpcyclops), java.util.stream.IntStream.of(p)).toArray();
            }
            if (ps == reverseStr(ps)) {
                ppcyclops = java.util.stream.IntStream.concat(java.util.Arrays.stream(ppcyclops), java.util.stream.IntStream.of(p)).toArray();
            }
        }
        System.out.println("\n\nThe first 50 blind prime cyclops numbers are:");
        idx = 0;
        while (idx < 50) {
            System.out.println(String.valueOf(pad(String.valueOf(commatize(bpcyclops[idx])), 6)) + " ");
            idx = idx + 1;
            if (Math.floorMod(idx, 10) == 0) {
                System.out.println("\n");
            }
        }
        int[] fb = findFirst(bpcyclops);
        System.out.println("\nFirst such number > 10 million is " + String.valueOf(commatize(fb[0])) + " at zero-based index " + String.valueOf(commatize(fb[1])));
        System.out.println("\n\nThe first 50 palindromic prime cyclops numbers are:");
        idx = 0;
        while (idx < 50) {
            System.out.println(String.valueOf(pad(String.valueOf(commatize(ppcyclops[idx])), 9)) + " ");
            idx = idx + 1;
            if (Math.floorMod(idx, 8) == 0) {
                System.out.println("\n");
            }
        }
        int[] fpp = findFirst(ppcyclops);
        System.out.println("\n\nFirst such number > 10 million is " + String.valueOf(commatize(fpp[0])) + " at zero-based index " + String.valueOf(commatize(fpp[1])));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
}
