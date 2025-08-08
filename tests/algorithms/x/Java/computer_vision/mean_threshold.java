public class Main {
    static int[][] img;
    static int[][] result;

    static int[][] mean_threshold(int[][] image) {
        int height = image.length;
        int width = image[0].length;
        int total = 0;
        int i = 0;
        while (i < height) {
            int j = 0;
            while (j < width) {
                total = total + image[i][j];
                j = j + 1;
            }
            i = i + 1;
        }
        int mean = Math.floorDiv(total, (height * width));
        i = 0;
        while (i < height) {
            int j_1 = 0;
            while (j_1 < width) {
                if (image[i][j_1] > mean) {
image[i][j_1] = 255;
                } else {
image[i][j_1] = 0;
                }
                j_1 = j_1 + 1;
            }
            i = i + 1;
        }
        return image;
    }

    static void print_image(int[][] image) {
        int i_1 = 0;
        while (i_1 < image.length) {
            System.out.println(java.util.Arrays.toString(image[i_1]));
            i_1 = i_1 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            img = ((int[][])(new int[][]{new int[]{10, 200, 50}, new int[]{100, 150, 30}, new int[]{90, 80, 220}}));
            result = ((int[][])(mean_threshold(((int[][])(img)))));
            print_image(((int[][])(result)));
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
