public class Main {
    static int[] rgb;
    static double[] hsv;

    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double fmod(double a, double b) {
        return a - b * ((Number)(a / b)).intValue();
    }

    static int roundf(double x) {
        if (x >= 0.0) {
            return ((Number)(x + 0.5)).intValue();
        }
        return ((Number)(x - 0.5)).intValue();
    }

    static double maxf(double a, double b, double c) {
        double m = a;
        if (b > m) {
            m = b;
        }
        if (c > m) {
            m = c;
        }
        return m;
    }

    static double minf(double a, double b, double c) {
        double m_1 = a;
        if (b < m_1) {
            m_1 = b;
        }
        if (c < m_1) {
            m_1 = c;
        }
        return m_1;
    }

    static int[] hsv_to_rgb(double hue, double saturation, double value) {
        if (hue < 0.0 || hue > 360.0) {
            System.out.println("hue should be between 0 and 360");
            return new int[]{};
        }
        if (saturation < 0.0 || saturation > 1.0) {
            System.out.println("saturation should be between 0 and 1");
            return new int[]{};
        }
        if (value < 0.0 || value > 1.0) {
            System.out.println("value should be between 0 and 1");
            return new int[]{};
        }
        double chroma = value * saturation;
        double hue_section = hue / 60.0;
        double second_largest_component = chroma * (1.0 - absf(fmod(hue_section, 2.0) - 1.0));
        double match_value = value - chroma;
        int red = 0;
        int green = 0;
        int blue = 0;
        if (hue_section >= 0.0 && hue_section <= 1.0) {
            red = roundf(255.0 * (chroma + match_value));
            green = roundf(255.0 * (second_largest_component + match_value));
            blue = roundf(255.0 * match_value);
        } else         if (hue_section > 1.0 && hue_section <= 2.0) {
            red = roundf(255.0 * (second_largest_component + match_value));
            green = roundf(255.0 * (chroma + match_value));
            blue = roundf(255.0 * match_value);
        } else         if (hue_section > 2.0 && hue_section <= 3.0) {
            red = roundf(255.0 * match_value);
            green = roundf(255.0 * (chroma + match_value));
            blue = roundf(255.0 * (second_largest_component + match_value));
        } else         if (hue_section > 3.0 && hue_section <= 4.0) {
            red = roundf(255.0 * match_value);
            green = roundf(255.0 * (second_largest_component + match_value));
            blue = roundf(255.0 * (chroma + match_value));
        } else         if (hue_section > 4.0 && hue_section <= 5.0) {
            red = roundf(255.0 * (second_largest_component + match_value));
            green = roundf(255.0 * match_value);
            blue = roundf(255.0 * (chroma + match_value));
        } else {
            red = roundf(255.0 * (chroma + match_value));
            green = roundf(255.0 * match_value);
            blue = roundf(255.0 * (second_largest_component + match_value));
        }
        return new int[]{red, green, blue};
    }

    static double[] rgb_to_hsv(int red, int green, int blue) {
        if (red < 0 || red > 255) {
            System.out.println("red should be between 0 and 255");
            return new double[]{};
        }
        if (green < 0 || green > 255) {
            System.out.println("green should be between 0 and 255");
            return new double[]{};
        }
        if (blue < 0 || blue > 255) {
            System.out.println("blue should be between 0 and 255");
            return new double[]{};
        }
        double float_red = red / 255.0;
        double float_green = green / 255.0;
        double float_blue = blue / 255.0;
        double value = maxf(float_red, float_green, float_blue);
        double min_val = minf(float_red, float_green, float_blue);
        double chroma_1 = value - min_val;
        double saturation = value == 0.0 ? 0.0 : chroma_1 / value;
        double hue = 0;
        if (chroma_1 == 0.0) {
            hue = 0.0;
        } else         if (value == float_red) {
            hue = 60.0 * (0.0 + (float_green - float_blue) / chroma_1);
        } else         if (value == float_green) {
            hue = 60.0 * (2.0 + (float_blue - float_red) / chroma_1);
        } else {
            hue = 60.0 * (4.0 + (float_red - float_green) / chroma_1);
        }
        hue = fmod(hue + 360.0, 360.0);
        return new double[]{hue, saturation, value};
    }

    static boolean approximately_equal_hsv(double[] hsv1, double[] hsv2) {
        boolean check_hue = absf(hsv1[0] - hsv2[0]) < 0.2;
        boolean check_saturation = absf(hsv1[1] - hsv2[1]) < 0.002;
        boolean check_value = absf(hsv1[2] - hsv2[2]) < 0.002;
        return check_hue && check_saturation && check_value;
    }
    public static void main(String[] args) {
        rgb = ((int[])(hsv_to_rgb(180.0, 0.5, 0.5)));
        System.out.println(_p(rgb));
        hsv = ((double[])(rgb_to_hsv(64, 128, 128)));
        System.out.println(_p(hsv));
        System.out.println(_p(approximately_equal_hsv(((double[])(hsv)), ((double[])(new double[]{180.0, 0.5, 0.5})))));
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
