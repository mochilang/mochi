public class Main {

    static int[][] image() {
        return new int[][]{new int[]{0, 0, 10000}, new int[]{65535, 65535, 65535}, new int[]{65535, 65535, 65535}};
    }

    static int[] histogram(int[][] g, int bins) {
        if (bins <= 0) {
            bins = g[0].length;
        }
        int[] h = new int[]{};
        int i = 0;
        while (i < bins) {
            h = java.util.stream.IntStream.concat(java.util.Arrays.stream(h), java.util.stream.IntStream.of(0)).toArray();
            i = i + 1;
        }
        int y = 0;
        while (y < g.length) {
            int[] row = g[y];
            int x = 0;
            while (x < row.length) {
                int p = row[x];
                int idx = ((Number)(((p * (bins - 1)) / 65535))).intValue();
h[idx] = h[idx] + 1;
                x = x + 1;
            }
            y = y + 1;
        }
        return h;
    }

    static int medianThreshold(int[] h) {
        int lb = 0;
        int ub = h.length - 1;
        int lSum = 0;
        int uSum = 0;
        while (lb <= ub) {
            if (lSum + h[lb] < uSum + h[ub]) {
                lSum = lSum + h[lb];
                lb = lb + 1;
            } else {
                uSum = uSum + h[ub];
                ub = ub - 1;
            }
        }
        return ((Number)(((ub * 65535) / h.length))).intValue();
    }

    static int[][] threshold(int[][] g, int t) {
        int[][] out = new int[][]{};
        int y_1 = 0;
        while (y_1 < g.length) {
            int[] row_1 = g[y_1];
            int[] newRow = new int[]{};
            int x_1 = 0;
            while (x_1 < row_1.length) {
                if (row_1[x_1] < t) {
                    newRow = java.util.stream.IntStream.concat(java.util.Arrays.stream(newRow), java.util.stream.IntStream.of(0)).toArray();
                } else {
                    newRow = java.util.stream.IntStream.concat(java.util.Arrays.stream(newRow), java.util.stream.IntStream.of(65535)).toArray();
                }
                x_1 = x_1 + 1;
            }
            out = appendObj(out, newRow);
            y_1 = y_1 + 1;
        }
        return out;
    }

    static void printImage(int[][] g) {
        int y_2 = 0;
        while (y_2 < g.length) {
            int[] row_2 = g[y_2];
            String line = "";
            int x_2 = 0;
            while (x_2 < row_2.length) {
                if (row_2[x_2] == 0) {
                    line = line + "0";
                } else {
                    line = line + "1";
                }
                x_2 = x_2 + 1;
            }
            System.out.println(line);
            y_2 = y_2 + 1;
        }
    }

    static void main() {
        int[][] img = image();
        int[] h_1 = histogram(img, 0);
        System.out.println("Histogram: " + _p(h_1));
        int t = medianThreshold(h_1);
        System.out.println("Threshold: " + _p(t));
        int[][] bw = threshold(img, t);
        printImage(bw);
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
