public class Main {

    static String show(int[] xs) {
        String s = "";
        int i = 0;
        while (i < xs.length) {
            s = s + String.valueOf(xs[i]);
            if (i < xs.length - 1) {
                s = s + " ";
            }
            i = i + 1;
        }
        return s;
    }

    static int[] gen(int[] init, int n) {
        int[] b = init;
        int[] res = new int[]{};
        int sum = 0;
        for (int x : b) {
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(x)).toArray();
            sum = sum + x;
        }
        while (res.length < n) {
            int next = sum;
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(next)).toArray();
            sum = sum + next - b[0];
            b = java.util.stream.IntStream.concat(java.util.Arrays.stream(java.util.Arrays.copyOfRange(b, 1, b.length)), java.util.stream.IntStream.of(next)).toArray();
        }
        return res;
    }

    static void main() {
        int n = 10;
        System.out.println(" Fibonacci: " + String.valueOf(show(gen(new int[]{1, 1}, n))));
        System.out.println("Tribonacci: " + String.valueOf(show(gen(new int[]{1, 1, 2}, n))));
        System.out.println("Tetranacci: " + String.valueOf(show(gen(new int[]{1, 1, 2, 4}, n))));
        System.out.println("     Lucas: " + String.valueOf(show(gen(new int[]{2, 1}, n))));
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
