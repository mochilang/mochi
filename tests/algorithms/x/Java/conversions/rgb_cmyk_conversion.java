public class Main {

    static int round_int(double x) {
        return ((Number)((x + 0.5))).intValue();
    }

    static int[] rgb_to_cmyk(int r_input, int g_input, int b_input) {
        if (r_input < 0 || r_input >= 256 || g_input < 0 || g_input >= 256 || b_input < 0 || b_input >= 256) {
            throw new RuntimeException(String.valueOf("Expected int of the range 0..255"));
        }
        double r = (((Number)(r_input)).doubleValue()) / 255.0;
        double g = (((Number)(g_input)).doubleValue()) / 255.0;
        double b = (((Number)(b_input)).doubleValue()) / 255.0;
        double max_val = r;
        if (g > max_val) {
            max_val = g;
        }
        if (b > max_val) {
            max_val = b;
        }
        double k_float = 1.0 - max_val;
        if (k_float == 1.0) {
            return new int[]{0, 0, 0, 100};
        }
        double c_float = 100.0 * (1.0 - r - k_float) / (1.0 - k_float);
        double m_float = 100.0 * (1.0 - g - k_float) / (1.0 - k_float);
        double y_float = 100.0 * (1.0 - b - k_float) / (1.0 - k_float);
        double k_percent = 100.0 * k_float;
        int c = round_int(c_float);
        int m = round_int(m_float);
        int y = round_int(y_float);
        int k = round_int(k_percent);
        return new int[]{c, m, y, k};
    }
    public static void main(String[] args) {
        System.out.println(rgb_to_cmyk(255, 255, 255));
        System.out.println(rgb_to_cmyk(128, 128, 128));
        System.out.println(rgb_to_cmyk(0, 0, 0));
        System.out.println(rgb_to_cmyk(255, 0, 0));
        System.out.println(rgb_to_cmyk(0, 255, 0));
        System.out.println(rgb_to_cmyk(0, 0, 255));
    }
}
