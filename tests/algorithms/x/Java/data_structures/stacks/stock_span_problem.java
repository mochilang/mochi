public class Main {
    static int[] price;
    static int[] spans;

    static int[] calculation_span(int[] price) {
        int n = price.length;
        int[] st = ((int[])(new int[]{}));
        int[] span = ((int[])(new int[]{}));
        st = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(st), java.util.stream.IntStream.of(0)).toArray()));
        span = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(span), java.util.stream.IntStream.of(1)).toArray()));
        for (int i = 1; i < n; i++) {
            while (st.length > 0 && price[st[st.length - 1]] <= price[i]) {
                st = ((int[])(java.util.Arrays.copyOfRange(st, 0, st.length - 1)));
            }
            int s = st.length <= 0 ? i + 1 : i - st[st.length - 1];
            span = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(span), java.util.stream.IntStream.of(s)).toArray()));
            st = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(st), java.util.stream.IntStream.of(i)).toArray()));
        }
        return span;
    }

    static void print_array(int[] arr) {
        for (int i = 0; i < arr.length; i++) {
            System.out.println(arr[i]);
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            price = ((int[])(new int[]{10, 4, 5, 90, 120, 80}));
            spans = ((int[])(calculation_span(((int[])(price)))));
            print_array(((int[])(spans)));
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
