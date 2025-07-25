public class Main {

    static double floorf(double x) {
        int y = ((Number)(x)).intValue();
        return ((Number)(y)).doubleValue();
    }

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < s.length()) {
            if ((s.substring(i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String fmtF3(double x) {
        double y = floorf(x * 1000.0 + 0.5) / 1000.0;
        String s = String.valueOf(y);
        int dot = indexOf(s, ".");
        if (dot == 0 - 1) {
            s = String.valueOf(s + ".000");
        } else {
            int decs = s.length() - dot - 1;
            if (decs > 3) {
                s = s.substring(0, dot + 4);
            } else {
                while (decs < 3) {
                    s = String.valueOf(s + "0");
                    decs = decs + 1;
                }
            }
        }
        return s;
    }

    static String padFloat3(double x, int width) {
        String s = String.valueOf(fmtF3(x));
        while (s.length() < width) {
            s = String.valueOf(" " + s);
        }
        return s;
    }

    static double[] fib1000() {
        double a = 0.0;
        double b = 1.0;
        double[] res = new double[]{};
        int i = 0;
        while (i < 1000) {
            res = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(b)).toArray();
            double t = b;
            b = b + a;
            a = t;
            i = i + 1;
        }
        return res;
    }

    static int leadingDigit(double x) {
        if (x < 0.0) {
            x = -x;
        }
        while (x >= 10.0) {
            x = x / 10.0;
        }
        while (x > 0.0 && x < 1.0) {
            x = x * 10.0;
        }
        return ((Number)(x)).intValue();
    }

    static void show(double[] nums, String title) {
        int[] counts = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0};
        for (double n : nums) {
            int d = leadingDigit(n);
            if (d >= 1 && d <= 9) {
counts[d - 1] = counts[d - 1] + 1;
            }
        }
        double[] preds = new double[]{0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046};
        int total = nums.length;
        System.out.println(title);
        System.out.println("Digit  Observed  Predicted");
        int i = 0;
        while (i < 9) {
            double obs = (counts[i]) / (((Number)(total)).doubleValue());
            String line = String.valueOf(String.valueOf(String.valueOf("  " + String.valueOf(i + 1)) + "  " + padFloat3(obs, 9)) + "  " + padFloat3(preds[i], 8));
            System.out.println(line);
            i = i + 1;
        }
    }

    static void main() {
        show(fib1000(), "First 1000 Fibonacci numbers");
    }
    public static void main(String[] args) {
        main();
    }
}
