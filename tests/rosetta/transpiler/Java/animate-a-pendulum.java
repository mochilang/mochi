public class Main {
    static double PI = 3.141592653589793;
    static double L = 10.0;
    static double G = 9.81;
    static double dt = 0.2;
    static double phi0 = PI / 4.0;
    static double omega = sqrtApprox(G / L);
    static double t = 0.0;

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
        double term = 1.0;
        double sum = 1.0;
        int n = 1;
        while (n <= 10) {
            double denom = ((Number)(((2 * n - 1) * (2 * n)))).doubleValue();
            term = -term * x * x / denom;
            sum = sum + term;
            n = n + 1;
        }
        return sum;
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
        for (int step = 0; step < 10; step++) {
            double phi = phi0 * cosApprox(omega * t);
            int pos = ((Number)((10.0 * sinApprox(phi) + 0.5))).intValue();
            System.out.println(String.valueOf(pos));
            t = t + dt;
        }
    }
}
