public class Main {
    static double PI;
    static double L;
    static double G;
    static double dt;
    static double phi0;
    static double omega;
    static double t;

    static double sinApprox(double x) {
        double term = x;
        double sum = x;
        int n = 1;
        while (n <= 10) {
            double denom = ((Number)(((2 * n) * (2 * n + 1)))).doubleValue();
            term = -term * x * x / denom;
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double cosApprox(double x) {
        double term_1 = 1.0;
        double sum_1 = 1.0;
        int n_1 = 1;
        while (n_1 <= 10) {
            double denom_1 = ((Number)(((2 * n_1 - 1) * (2 * n_1)))).doubleValue();
            term_1 = -term_1 * x * x / denom_1;
            sum_1 = sum_1 + term_1;
            n_1 = n_1 + 1;
        }
        return sum_1;
    }

    static double sqrtApprox(double x) {
        double guess = x;
        int i = 0;
        while (i < 10) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }
    public static void main(String[] args) {
        PI = 3.141592653589793;
        L = 10.0;
        G = 9.81;
        dt = 0.2;
        phi0 = PI / 4.0;
        omega = sqrtApprox(G / L);
        t = 0.0;
        for (int step = 0; step < 10; step++) {
            double phi = phi0 * cosApprox(omega * t);
            int pos = ((Number)((10.0 * sinApprox(phi) + 0.5))).intValue();
            System.out.println(_p(pos));
            t = t + dt;
        }
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
