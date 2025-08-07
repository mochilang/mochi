public class Main {

    static int[] hamming(int n) {
        if (n < 1) {
            throw new RuntimeException(String.valueOf("n_element should be a positive number"));
        }
        int[] hamming_list = ((int[])(new int[]{1}));
        int i = 0;
        int j = 0;
        int k = 0;
        int index = 1;
        while (index < n) {
            while (hamming_list[i] * 2 <= hamming_list[hamming_list.length - 1]) {
                i = i + 1;
            }
            while (hamming_list[j] * 3 <= hamming_list[hamming_list.length - 1]) {
                j = j + 1;
            }
            while (hamming_list[k] * 5 <= hamming_list[hamming_list.length - 1]) {
                k = k + 1;
            }
            int m1 = hamming_list[i] * 2;
            int m2 = hamming_list[j] * 3;
            int m3 = hamming_list[k] * 5;
            int next = m1;
            if (m2 < next) {
                next = m2;
            }
            if (m3 < next) {
                next = m3;
            }
            hamming_list = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(hamming_list), java.util.stream.IntStream.of(next)).toArray()));
            index = index + 1;
        }
        return hamming_list;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(hamming(5));
            System.out.println(hamming(10));
            System.out.println(hamming(15));
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
