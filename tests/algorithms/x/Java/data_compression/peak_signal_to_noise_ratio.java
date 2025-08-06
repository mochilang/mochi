public class Main {
    static double abs(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 10) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double ln(double x) {
        double t = (x - 1.0) / (x + 1.0);
        double term = t;
        double sum = 0.0;
        int n = 1;
        while (n <= 19) {
            sum = sum + term / (((Number)(n)).doubleValue());
            term = term * t * t;
            n = n + 2;
        }
        return 2.0 * sum;
    }

    static double log10(double x) {
        return ln(x) / ln(10.0);
    }

    static double peak_signal_to_noise_ratio(int[][] original, int[][] contrast) {
        double mse = 0.0;
        int i_1 = 0;
        while (i_1 < original.length) {
            int j = 0;
            while (j < original[i_1].length) {
                double diff = ((Number)((original[i_1][j] - contrast[i_1][j]))).doubleValue();
                mse = mse + diff * diff;
                j = j + 1;
            }
            i_1 = i_1 + 1;
        }
        double size = ((Number)((original.length * original[0].length))).doubleValue();
        mse = mse / size;
        if (mse == 0.0) {
            return 100.0;
        }
        double PIXEL_MAX = 255.0;
        return 20.0 * log10(PIXEL_MAX / sqrtApprox(mse));
    }
    public static void main(String[] args) {
    }
}
