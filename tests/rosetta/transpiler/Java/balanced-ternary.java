public class Main {

    static String trimLeftZeros(String s) {
        int i = 0;
        while (i < s.length() && s.substring(i, i + 1) == "0") {
            i = i + 1;
        }
        return s.substring(i, s.length());
    }

    static java.util.Map<String,Object> btString(String s) {
        s = String.valueOf(trimLeftZeros(s));
        int[] b = new int[]{};
        int i = s.length() - 1;
        while (i >= 0) {
            String ch = s.substring(i, i + 1);
            if ((ch.equals("+"))) {
                b = java.util.stream.IntStream.concat(java.util.Arrays.stream(b), java.util.stream.IntStream.of(1)).toArray();
            } else             if ((ch.equals("0"))) {
                b = java.util.stream.IntStream.concat(java.util.Arrays.stream(b), java.util.stream.IntStream.of(0)).toArray();
            } else             if ((ch.equals("-"))) {
                b = java.util.stream.IntStream.concat(java.util.Arrays.stream(b), java.util.stream.IntStream.of(0 - 1)).toArray();
            } else {
                return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("bt", new Object[]{}), java.util.Map.entry("ok", false)));
            }
            i = i - 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("bt", b), java.util.Map.entry("ok", true)));
    }

    static String btToString(int[] b) {
        if (b.length == 0) {
            return "0";
        }
        String r = "";
        int i = b.length - 1;
        while (i >= 0) {
            int d = b[i];
            if (d == 0 - 1) {
                r = String.valueOf(r + "-");
            } else             if (d == 0) {
                r = String.valueOf(r + "0");
            } else {
                r = String.valueOf(r + "+");
            }
            i = i - 1;
        }
        return r;
    }

    static int[] btInt(int i) {
        if (i == 0) {
            return new int[]{};
        }
        int n = i;
        int[] b = new int[]{};
        while (n != 0) {
            int m = Math.floorMod(n, 3);
            n = ((Number)((n / 3))).intValue();
            if (m == 2) {
                m = 0 - 1;
                n = n + 1;
            } else             if (m == 0 - 2) {
                m = 1;
                n = n - 1;
            }
            b = java.util.stream.IntStream.concat(java.util.Arrays.stream(b), java.util.stream.IntStream.of(m)).toArray();
        }
        return b;
    }

    static int btToInt(int[] b) {
        int r = 0;
        int pt = 1;
        int i = 0;
        while (i < b.length) {
            r = r + b[i] * pt;
            pt = pt * 3;
            i = i + 1;
        }
        return r;
    }

    static int[] btNeg(int[] b) {
        int[] r = new int[]{};
        int i = 0;
        while (i < b.length) {
            r = java.util.stream.IntStream.concat(java.util.Arrays.stream(r), java.util.stream.IntStream.of(-b[i])).toArray();
            i = i + 1;
        }
        return r;
    }

    static int[] btAdd(int[] a, int[] b) {
        return btInt(btToInt(a) + btToInt(b));
    }

    static int[] btMul(int[] a, int[] b) {
        return btInt(btToInt(a) * btToInt(b));
    }

    static String padLeft(String s, int w) {
        String r = s;
        while (r.length() < w) {
            r = String.valueOf(" " + r);
        }
        return r;
    }

    static void show(String label, int[] b) {
        String l = String.valueOf(padLeft(label, 7));
        String bs = String.valueOf(padLeft(String.valueOf(btToString(b)), 12));
        String is = String.valueOf(padLeft(String.valueOf(btToInt(b)), 7));
        System.out.println(String.valueOf(String.valueOf(String.valueOf(l + " ") + bs) + " ") + is);
    }

    static void main() {
        java.util.Map<String,Object> ares = btString("+-0++0+");
        int[] a = (int[])(((int[])ares.get("bt")));
        int[] b = btInt(-436);
        java.util.Map<String,Object> cres = btString("+-++-");
        int[] c = (int[])(((int[])cres.get("bt")));
        show("a:", a);
        show("b:", b);
        show("c:", c);
        show("a(b-c):", btMul(a, btAdd(b, btNeg(c))));
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
}
