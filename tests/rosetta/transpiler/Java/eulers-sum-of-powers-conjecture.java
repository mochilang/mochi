public class Main {

    static int[] eulerSum() {
        int[] pow5 = new int[]{};
        int i = 0;
        while (i < 250) {
            pow5 = java.util.stream.IntStream.concat(java.util.Arrays.stream(pow5), java.util.stream.IntStream.of(i * i * i * i * i)).toArray();
            i = i + 1;
        }
        java.util.Map<Integer,int[]> sums = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>()));
        int x2 = 2;
        while (x2 < 250) {
            int x3 = 1;
            while (x3 < x2) {
                int s = pow5[x2] + pow5[x3];
                if (!(Boolean)(sums.containsKey(s))) {
sums.put(s, new int[]{x2, x3});
                }
                x3 = x3 + 1;
            }
            x2 = x2 + 1;
        }
        int x0 = 4;
        while (x0 < 250) {
            int x1 = 3;
            while (x1 < x0) {
                int y = x0 + 1;
                while (y < 250) {
                    int rem = pow5[y] - pow5[x0] - pow5[x1];
                    if (sums.containsKey(rem)) {
                        int[] pair = (int[])(((int[])(sums).get(rem)));
                        int a = pair[0];
                        int b = pair[1];
                        if (x1 > a && a > b) {
                            return new int[]{x0, x1, a, b, y};
                        }
                    }
                    y = y + 1;
                }
                x1 = x1 + 1;
            }
            x0 = x0 + 1;
        }
        return new int[]{0, 0, 0, 0, 0};
    }

    static void main() {
        int[] r = eulerSum();
        System.out.println((String)(_p(_geti(r, 0))) + " " + (String)(_p(_geti(r, 1))) + " " + (String)(_p(_geti(r, 2))) + " " + (String)(_p(_geti(r, 3))) + " " + (String)(_p(_geti(r, 4))));
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
