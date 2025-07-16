// active-object.mochi
public class ActiveObject {
    static double sinApprox(double x) {
        double term = x;
        double sum = x;
        int n = 1;
        while (n <= 12) {
            double denom = Double.parseDouble(String.valueOf(((2 * n) * (2 * n + 1))));
            term = -term * x * x / denom;
            sum = sum + term;
            n = (int)(n + 1);
        }
        return sum;
    }
    public static void main(String[] args) {
    double PI = 3.141593;
    double dt = 0.010000;
    double s = 0.000000;
    double t1 = 0.000000;
    double k1 = sinApprox(0.000000);
    int i = 1;
    while (i <= 200) {
        double t2 = (Double.parseDouble(String.valueOf(i))) * dt;
        double k2 = sinApprox(t2 * PI);
        s = s + (k1 + k2) * 0.500000 * (t2 - t1);
        t1 = t2;
        k1 = k2;
        i = (int)(i + 1);
    }
    int i2 = 1;
    while (i2 <= 50) {
        double t2 = 2.000000 + (Double.parseDouble(String.valueOf(i2))) * dt;
        double k2 = 0.000000;
        s = s + (k1 + k2) * 0.500000 * (t2 - t1);
        t1 = t2;
        k1 = k2;
        i2 = (int)(i2 + 1);
    }
    System.out.println(s);
    }
}
