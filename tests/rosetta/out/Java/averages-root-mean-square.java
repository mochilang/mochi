// averages-root-mean-square.mochi
public class AveragesRootMeanSquare {
    static int x = 1;
    static double sqrtApprox(double x) {
        double guess = x;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.000000;
            i = (int)(i + 1);
        }
        return guess;
    }
    public static void main(String[] args) {
    int n = 10;
    double sum = 0.000000;
    while (x <= n) {
        sum = sum + (Double.parseDouble(String.valueOf(x))) * (Double.parseDouble(String.valueOf(x)));
        x = (int)(x + 1);
    }
    double rms = sqrtApprox(sum / (Double.parseDouble(String.valueOf(n))));
    System.out.println(String.valueOf(rms));
    }
}
