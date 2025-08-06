public class Main {
    static class RGB {
        int r;
        int g;
        int b;
        RGB(int r, int g, int b) {
            this.r = r;
            this.g = g;
            this.b = b;
        }
        RGB() {}
        @Override public String toString() {
            return String.format("{'r': %s, 'g': %s, 'b': %s}", String.valueOf(r), String.valueOf(g), String.valueOf(b));
        }
    }

    static RGB[][] img1;
    static RGB[][] img2;

    static int round_int(double x) {
        return ((Number)((x + 0.5))).intValue();
    }

    static RGB hsv_to_rgb(double h, double s, double v) {
        int i = ((Number)((h * 6.0))).intValue();
        double f = h * 6.0 - (((Number)(i)).doubleValue());
        double p = v * (1.0 - s);
        double q = v * (1.0 - f * s);
        double t = v * (1.0 - (1.0 - f) * s);
        int mod = Math.floorMod(i, 6);
        double r = 0.0;
        double g = 0.0;
        double b = 0.0;
        if (mod == 0) {
            r = v;
            g = t;
            b = p;
        } else         if (mod == 1) {
            r = q;
            g = v;
            b = p;
        } else         if (mod == 2) {
            r = p;
            g = v;
            b = t;
        } else         if (mod == 3) {
            r = p;
            g = q;
            b = v;
        } else         if (mod == 4) {
            r = t;
            g = p;
            b = v;
        } else {
            r = v;
            g = p;
            b = q;
        }
        return new RGB(round_int(r * 255.0), round_int(g * 255.0), round_int(b * 255.0));
    }

    static double get_distance(double x, double y, int max_step) {
        double a = x;
        double b_1 = y;
        int step = -1;
        while (step < max_step - 1) {
            step = step + 1;
            double a_new = a * a - b_1 * b_1 + x;
            b_1 = 2.0 * a * b_1 + y;
            a = a_new;
            if (a * a + b_1 * b_1 > 4.0) {
                break;
            }
        }
        return (((Number)(step)).doubleValue()) / (((Number)((max_step - 1))).doubleValue());
    }

    static RGB get_black_and_white_rgb(double distance) {
        if (distance == 1.0) {
            return new RGB(0, 0, 0);
        } else {
            return new RGB(255, 255, 255);
        }
    }

    static RGB get_color_coded_rgb(double distance) {
        if (distance == 1.0) {
            return new RGB(0, 0, 0);
        } else {
            return hsv_to_rgb(distance, 1.0, 1.0);
        }
    }

    static RGB[][] get_image(int image_width, int image_height, double figure_center_x, double figure_center_y, double figure_width, int max_step, boolean use_distance_color_coding) {
        RGB[][] img = ((RGB[][])(new RGB[][]{}));
        double figure_height = figure_width / (((Number)(image_width)).doubleValue()) * (((Number)(image_height)).doubleValue());
        int image_y = 0;
        while (image_y < image_height) {
            RGB[] row = ((RGB[])(new RGB[]{}));
            int image_x = 0;
            while (image_x < image_width) {
                double fx = figure_center_x + ((((Number)(image_x)).doubleValue()) / (((Number)(image_width)).doubleValue()) - 0.5) * figure_width;
                double fy = figure_center_y + ((((Number)(image_y)).doubleValue()) / (((Number)(image_height)).doubleValue()) - 0.5) * figure_height;
                double distance = get_distance(fx, fy, max_step);
                RGB rgb = null;
                if (((Boolean)(use_distance_color_coding))) {
                    rgb = get_color_coded_rgb(distance);
                } else {
                    rgb = get_black_and_white_rgb(distance);
                }
                row = ((RGB[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of(rgb)).toArray(RGB[]::new)));
                image_x = image_x + 1;
            }
            img = ((RGB[][])(appendObj(img, row)));
            image_y = image_y + 1;
        }
        return img;
    }

    static String rgb_to_string(RGB c) {
        return "(" + _p(c.r) + ", " + _p(c.g) + ", " + _p(c.b) + ")";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            img1 = ((RGB[][])(get_image(10, 10, -0.6, 0.0, 3.2, 50, true)));
            System.out.println(rgb_to_string(img1[0][0]));
            img2 = ((RGB[][])(get_image(10, 10, -0.6, 0.0, 3.2, 50, false)));
            System.out.println(rgb_to_string(img2[0][0]));
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
