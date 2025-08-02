public class Main {

    static double makeInf() {
        double x = 1.0;
        int i = 0;
        while (i < 400) {
            x = x * 10.0;
            i = i + 1;
        }
        return x;
    }

    static double makeMax() {
        double x_1 = 1.0;
        int i_1 = 0;
        while (i_1 < 308) {
            x_1 = x_1 * 10.0;
            i_1 = i_1 + 1;
        }
        return x_1;
    }

    static boolean isNaN(double x) {
        return x != x;
    }

    static void validateNaN(double n, String op) {
        if (((Boolean)(isNaN(n)))) {
            System.out.println(op + " -> NaN");
        } else {
            System.out.println("!!! Expected NaN from" + " " + op + " " + " Found" + " " + String.valueOf(n));
        }
    }

    static void validateZero(double n, String op) {
        if (n == 0) {
            System.out.println(op + " -> 0");
        } else {
            System.out.println("!!! Expected 0 from" + " " + op + " " + " Found" + " " + String.valueOf(n));
        }
    }

    static void validateGT(double a, double b, String op) {
        if (a > b) {
            System.out.println(op);
        } else {
            System.out.println("!!! Expected" + " " + op + " " + " Found not true.");
        }
    }

    static void validateNE(double a, double b, String op) {
        if (a == b) {
            System.out.println("!!! Expected" + " " + op + " " + " Found not true.");
        } else {
            System.out.println(op);
        }
    }

    static void validateEQ(double a, double b, String op) {
        if (a == b) {
            System.out.println(op);
        } else {
            System.out.println("!!! Expected" + " " + op + " " + " Found not true.");
        }
    }

    static void main() {
        double negZero = -0.0;
        double posInf = makeInf();
        double negInf = -posInf;
        double nan = posInf / posInf;
        double maxVal = makeMax();
        System.out.println(String.valueOf(negZero) + " " + String.valueOf(posInf) + " " + String.valueOf(negInf) + " " + String.valueOf(nan));
        System.out.println(String.valueOf(negZero) + " " + String.valueOf(posInf) + " " + String.valueOf(negInf) + " " + String.valueOf(nan));
        System.out.println("");
        validateNaN(negInf + posInf, "-Inf + Inf");
        validateNaN(0.0 * posInf, "0 * Inf");
        validateNaN(posInf / posInf, "Inf / Inf");
        validateNaN(posInf % 1.0, "Inf % 1");
        validateNaN(1.0 + nan, "1 + NaN");
        validateZero(1.0 / posInf, "1 / Inf");
        validateGT(posInf, maxVal, "Inf > max value");
        validateGT(-maxVal, negInf, "-Inf < max neg value");
        validateNE(nan, nan, "NaN != NaN");
        validateEQ(negZero, 0.0, "-0 == 0");
    }
    public static void main(String[] args) {
        main();
    }
}
