public class Main {

    static double floor(double x) {
        int i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(int n) {
        double p = 1.0;
        int i_1 = 0;
        while (i_1 < n) {
            p = p * 10.0;
            i_1 = i_1 + 1;
        }
        return p;
    }

    static double round_to(double x, int ndigits) {
        double m = pow10(ndigits);
        return floor(x * m + 0.5) / m;
    }

    static double celsius_to_fahrenheit(double c, int ndigits) {
        return round_to(c * 9.0 / 5.0 + 32.0, ndigits);
    }

    static double celsius_to_kelvin(double c, int ndigits) {
        return round_to(c + 273.15, ndigits);
    }

    static double celsius_to_rankine(double c, int ndigits) {
        return round_to(c * 9.0 / 5.0 + 491.67, ndigits);
    }

    static double fahrenheit_to_celsius(double f, int ndigits) {
        return round_to((f - 32.0) * 5.0 / 9.0, ndigits);
    }

    static double fahrenheit_to_kelvin(double f, int ndigits) {
        return round_to((f - 32.0) * 5.0 / 9.0 + 273.15, ndigits);
    }

    static double fahrenheit_to_rankine(double f, int ndigits) {
        return round_to(f + 459.67, ndigits);
    }

    static double kelvin_to_celsius(double k, int ndigits) {
        return round_to(k - 273.15, ndigits);
    }

    static double kelvin_to_fahrenheit(double k, int ndigits) {
        return round_to((k - 273.15) * 9.0 / 5.0 + 32.0, ndigits);
    }

    static double kelvin_to_rankine(double k, int ndigits) {
        return round_to(k * 9.0 / 5.0, ndigits);
    }

    static double rankine_to_celsius(double r, int ndigits) {
        return round_to((r - 491.67) * 5.0 / 9.0, ndigits);
    }

    static double rankine_to_fahrenheit(double r, int ndigits) {
        return round_to(r - 459.67, ndigits);
    }

    static double rankine_to_kelvin(double r, int ndigits) {
        return round_to(r * 5.0 / 9.0, ndigits);
    }

    static double reaumur_to_kelvin(double r, int ndigits) {
        return round_to(r * 1.25 + 273.15, ndigits);
    }

    static double reaumur_to_fahrenheit(double r, int ndigits) {
        return round_to(r * 2.25 + 32.0, ndigits);
    }

    static double reaumur_to_celsius(double r, int ndigits) {
        return round_to(r * 1.25, ndigits);
    }

    static double reaumur_to_rankine(double r, int ndigits) {
        return round_to(r * 2.25 + 32.0 + 459.67, ndigits);
    }
    public static void main(String[] args) {
        System.out.println(celsius_to_fahrenheit(0.0, 2));
        System.out.println(celsius_to_kelvin(0.0, 2));
        System.out.println(celsius_to_rankine(0.0, 2));
        System.out.println(fahrenheit_to_celsius(32.0, 2));
        System.out.println(fahrenheit_to_kelvin(32.0, 2));
        System.out.println(fahrenheit_to_rankine(32.0, 2));
        System.out.println(kelvin_to_celsius(273.15, 2));
        System.out.println(kelvin_to_fahrenheit(273.15, 2));
        System.out.println(kelvin_to_rankine(273.15, 2));
        System.out.println(rankine_to_celsius(491.67, 2));
        System.out.println(rankine_to_fahrenheit(491.67, 2));
        System.out.println(rankine_to_kelvin(491.67, 2));
        System.out.println(reaumur_to_kelvin(80.0, 2));
        System.out.println(reaumur_to_fahrenheit(80.0, 2));
        System.out.println(reaumur_to_celsius(80.0, 2));
        System.out.println(reaumur_to_rankine(80.0, 2));
    }
}
