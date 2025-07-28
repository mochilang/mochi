public class Main {

    static String padLeft(int n, int width) {
        String s = String.valueOf(n);
        while (_runeLen(s) < width) {
            s = " " + s;
        }
        return s;
    }

    static int modPow(int base, int exp, int mod) {
        int result = Math.floorMod(1, mod);
        int b = Math.floorMod(base, mod);
        int e = exp;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result = Math.floorMod((result * b), mod);
            }
            b = Math.floorMod((b * b), mod);
            e = e / 2;
        }
        return result;
    }

    static void main() {
        int k = 2;
        while (k <= 10) {
            System.out.println("The first 50 Curzon numbers using a base of " + String.valueOf(k) + " :");
            int count = 0;
            int n = 1;
            int[] curzon50 = new int[]{};
            while (true) {
                int d = k * n + 1;
                if (Math.floorMod((modPow(k, n, d) + 1), d) == 0) {
                    if (count < 50) {
                        curzon50 = java.util.stream.IntStream.concat(java.util.Arrays.stream(curzon50), java.util.stream.IntStream.of(n)).toArray();
                    }
                    count = count + 1;
                    if (count == 50) {
                        int idx = 0;
                        while (idx < curzon50.length) {
                            String line = "";
                            int j = 0;
                            while (j < 10) {
                                line = line + String.valueOf(padLeft(curzon50[idx], 4)) + " ";
                                idx = idx + 1;
                                j = j + 1;
                            }
                            System.out.println(_substr(line, 0, _runeLen(line) - 1));
                        }
                    }
                    if (count == 1000) {
                        System.out.println("\nOne thousandth: " + String.valueOf(n));
                        break;
                    }
                }
                n = n + 1;
            }
            System.out.println("");
            k = k + 2;
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
}
