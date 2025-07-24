public class Main {
    static double PI = 3.141592653589793;
    static double dt = 0.01;
    static double s = 0.0;
    static double t1 = 0.0;
    static double k1 = sinApprox(0.0);
    static int i = 1;
    static int i2 = 1;

    static double sinApprox(double x) {
        double term = x;
        double sum = x;
        int n = 1;
        while (n <= 12) {
            double denom = ((Number)(((2 * n) * (2 * n + 1)))).doubleValue();
            term = -term * x * x / denom;
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }
    public static void main(String[] args) {
        while (i <= 200) {
            double t2 = (((Number)(i)).doubleValue()) * dt;
            double k2 = sinApprox(t2 * PI);
            s = s + (k1 + k2) * 0.5 * (t2 - t1);
            t1 = t2;
            k1 = k2;
            i = i + 1;
        }
        while (i2 <= 50) {
            double t2 = 2.0 + (((Number)(i2)).doubleValue()) * dt;
            double k2 = 0.0;
            s = s + (k1 + k2) * 0.5 * (t2 - t1);
            t1 = t2;
            k1 = k2;
            i2 = i2 + 1;
        }
        System.out.println(s);
    }
}
