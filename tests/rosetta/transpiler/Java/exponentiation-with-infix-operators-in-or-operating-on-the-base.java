public class Main {
    static String[] ops;

    static double p(double x, double e) {
        double r = 1.0;
        int i = 0;
        while (i < (((Number)(e)).intValue())) {
            r = r * x;
            i = i + 1;
        }
        return r;
    }

    static String padInt(double f) {
        String s = (String)(_p((((Number)(f)).intValue())));
        if (f >= 0) {
            return " " + s;
        }
        return s;
    }
    public static void main(String[] args) {
        ops = new String[]{"-x.p(e)", "-(x).p(e)", "(-x).p(e)", "-(x.p(e))"};
        for (double x : new double[]{-5.0, 5.0}) {
            for (double e : new double[]{2.0, 3.0}) {
                double a = -p(x, e);
                double b = -(p(x, e));
                double c = p(-x, e);
                double d = -(p(x, e));
                System.out.println("x = " + String.valueOf((x < 0 ? "" : " ")) + (String)(_p((((Number)(x)).intValue()))) + " e = " + (String)(_p((((Number)(e)).intValue()))) + " | " + ops[0] + " = " + String.valueOf(padInt(a)) + " | " + ops[1] + " = " + String.valueOf(padInt(b)) + " | " + ops[2] + " = " + String.valueOf(padInt(c)) + " | " + ops[3] + " = " + String.valueOf(padInt(d)));
            }
        }
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
