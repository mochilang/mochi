// calculating-the-value-of-e.mochi
public class CalculatingTheValueOfE {
    static int n = 2;
    static double absf(double x) {
        if (x < 0.000000) {
            return -x;
        }
        return x;
    }
    static double pow10(int n) {
        double r = 1.000000;
        int i = 0;
        while (i < n) {
            r = r * 10.000000;
            i = (int)(i + 1);
        }
        return r;
    }
    static String formatFloat(double f, int prec) {
        double scale = pow10(prec);
        double scaled = (f * scale) + 0.500000;
        Object n = (Integer.parseInt(scaled));
        String digits = String.valueOf(n);
        while (digits.length() <= prec) {
            digits = "0" + digits;
        }
        String intPart = digits.substring(0, digits.length() - prec);
        String fracPart = digits.substring(digits.length() - prec, digits.length());
        return intPart + "." + fracPart;
    }
    public static void main(String[] args) {
    double epsilon = 0.000000;
    int factval = 1;
    double e = 2.000000;
    double term = 1.000000;
    while (true) {
        factval = (int)(factval * n);
        n = (int)(n + 1);
        term = 1.000000 / (Double.parseDouble(String.valueOf(factval)));
        e = e + term;
        if (absf(term) < epsilon) {
            break;
        }
    }
    System.out.println("e = " + formatFloat(e, 15));
    }
}
