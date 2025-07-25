public class Main {
    static int[] arr1 = new int[]{2, 7, 1, 8, 2};
    static java.util.Map<Integer,Integer> counts1 = new java.util.LinkedHashMap<Integer, Integer>();
    static int[] keys1 = new int[]{};
    static int i = 0;
    static int max1 = 0;
    static int[] modes1 = new int[]{};
    static int[] arr2 = new int[]{2, 7, 1, 8, 2, 8};
    static java.util.Map<Integer,Integer> counts2 = new java.util.LinkedHashMap<Integer, Integer>();
    static int[] keys2 = new int[]{};
    static int max2 = 0;
    static int[] modes2 = new int[]{};

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            while (i < arr1.length) {
                int v = arr1[i];
                if (counts1.containsKey(v)) {
counts1.put(v, (int)(((int)counts1.getOrDefault(v, 0))) + 1);
                } else {
counts1.put(v, 1);
                    keys1 = java.util.stream.IntStream.concat(java.util.Arrays.stream(keys1), java.util.stream.IntStream.of(v)).toArray();
                }
                i = i + 1;
            }
            i = 0;
            while (i < keys1.length) {
                int k = keys1[i];
                int c = (int)(((int)counts1.getOrDefault(k, 0)));
                if (c > max1) {
                    max1 = c;
                }
                i = i + 1;
            }
            i = 0;
            while (i < keys1.length) {
                int k = keys1[i];
                if ((int)(((int)counts1.getOrDefault(k, 0))) == max1) {
                    modes1 = java.util.stream.IntStream.concat(java.util.Arrays.stream(modes1), java.util.stream.IntStream.of(k)).toArray();
                }
                i = i + 1;
            }
            System.out.println(String.valueOf(modes1));
            i = 0;
            while (i < arr2.length) {
                int v = arr2[i];
                if (counts2.containsKey(v)) {
counts2.put(v, (int)(((int)counts2.getOrDefault(v, 0))) + 1);
                } else {
counts2.put(v, 1);
                    keys2 = java.util.stream.IntStream.concat(java.util.Arrays.stream(keys2), java.util.stream.IntStream.of(v)).toArray();
                }
                i = i + 1;
            }
            i = 0;
            while (i < keys2.length) {
                int k = keys2[i];
                int c = (int)(((int)counts2.getOrDefault(k, 0)));
                if (c > max2) {
                    max2 = c;
                }
                i = i + 1;
            }
            i = 0;
            while (i < keys2.length) {
                int k = keys2[i];
                if ((int)(((int)counts2.getOrDefault(k, 0))) == max2) {
                    modes2 = java.util.stream.IntStream.concat(java.util.Arrays.stream(modes2), java.util.stream.IntStream.of(k)).toArray();
                }
                i = i + 1;
            }
            System.out.println(String.valueOf(modes2));
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
