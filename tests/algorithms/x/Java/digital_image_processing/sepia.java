public class Main {
    static int[][][] image = new int[0][][];
    static int[][][] sepia;

    static int normalize(int value) {
        if (value > 255) {
            return 255;
        }
        return value;
    }

    static int to_grayscale(int blue, int green, int red) {
        double gs = 0.2126 * (((Number)(red)).doubleValue()) + 0.587 * (((Number)(green)).doubleValue()) + 0.114 * (((Number)(blue)).doubleValue());
        return ((Number)(gs)).intValue();
    }

    static int[][][] make_sepia(int[][][] img, int factor) {
        int pixel_h = img.length;
        int pixel_v = img[0].length;
        int i = 0;
        while (i < pixel_h) {
            int j = 0;
            while (j < pixel_v) {
                int[] pixel = ((int[])(img[i][j]));
                int grey = to_grayscale(pixel[0], pixel[1], pixel[2]);
img[i][j] = ((int[])(new int[]{normalize(grey), normalize(grey + factor), normalize(grey + 2 * factor)}));
                j = j + 1;
            }
            i = i + 1;
        }
        return img;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            image = ((int[][][])(new int[][][]{new int[][]{new int[]{10, 20, 30}, new int[]{40, 50, 60}}, new int[][]{new int[]{70, 80, 90}, new int[]{200, 150, 100}}}));
            sepia = ((int[][][])(make_sepia(((int[][][])(image)), 20)));
            System.out.println(_p(sepia));
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
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
