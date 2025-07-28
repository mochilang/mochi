public class Main {
    static int width = 320;
    static int height = 240;
    static String[][] img = new String[][]{};
    static int y = 0;

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            while (y < height) {
                String[] row = new String[]{};
                int x = 0;
                while (x < width) {
                    row = java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of("green")).toArray(String[]::new);
                    x = x + 1;
                }
                img = appendObj(img, row);
                y = y + 1;
            }
img[100][100] = "red";
            System.out.println("The color of the pixel at (  0,   0) is " + img[0][0] + ".");
            System.out.println("The color of the pixel at (100, 100) is " + img[100][100] + ".");
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
}
