public class Main {
    static class Fps {
        double[] coeffs;
        java.util.function.Function<Integer,Double> compute;
        Fps(double[] coeffs, java.util.function.Function<Integer,Double> compute) {
            this.coeffs = coeffs;
            this.compute = compute;
        }
        @Override public String toString() {
            return String.format("{'coeffs': %s, 'compute': %s}", String.valueOf(coeffs), String.valueOf(compute));
        }
    }

    static class Pair {
        Fps sin;
        Fps cos;
        Pair(Fps sin, Fps cos) {
            this.sin = sin;
            this.cos = cos;
        }
        @Override public String toString() {
            return String.format("{'sin': %s, 'cos': %s}", String.valueOf(sin), String.valueOf(cos));
        }
    }


    static Fps newFps(java.util.function.Function<Integer,Double> fn) {
        return new Fps(new double[]{}, fn);
    }

    static double extract(Fps f, int n) {
        while (f.coeffs.length <= n) {
            int idx = f.coeffs.length;
            Object v = f.compute.apply(idx);
f.coeffs = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(f.coeffs), java.util.stream.DoubleStream.of(((Number)(v)).doubleValue())).toArray();
        }
        return f.coeffs[n];
    }

    static Fps one() {
        return newFps((i) -> {
        if (i == 0) {
            return 1.0;
        }
        return 0.0;
});
    }

    static Fps add(Fps a, Fps b) {
        return newFps((n) -> extract(a, n) + extract(b, n));
    }

    static Fps sub(Fps a, Fps b) {
        return newFps((n_1) -> extract(a, n_1) - extract(b, n_1));
    }

    static Fps mul(Fps a, Fps b) {
        return newFps((n_2) -> {
        double s = 0.0;
        int k = 0;
        while (k <= n_2) {
            s = s + extract(a, k) * extract(b, n_2 - k);
            k = k + 1;
        }
        return s;
});
    }

    static Fps div(Fps a, Fps b) {
        Fps q = newFps((n_3) -> 0.0);
q.compute = (n_4) -> {
        double b0 = extract(b, 0);
        if (b0 == 0.0) {
            return (0.0 / 0.0);
        }
        double s_1 = extract(a, n_4);
        int k_1 = 1;
        while (k_1 <= n_4) {
            s_1 = s_1 - extract(b, k_1) * extract(q, n_4 - k_1);
            k_1 = k_1 + 1;
        }
        return s_1 / b0;
};
        return q;
    }

    static Fps differentiate(Fps a) {
        return newFps((n_5) -> (((Number)((n_5 + 1))).doubleValue()) * extract(a, n_5 + 1));
    }

    static Fps integrate(Fps a) {
        return newFps((n_6) -> {
        if (n_6 == 0) {
            return 0.0;
        }
        return extract(a, n_6 - 1) / (((Number)(n_6)).doubleValue());
});
    }

    static Pair sinCos() {
        Fps sin = newFps((n_7) -> 0.0);
        Fps cos = sub(one(), integrate(sin));
sin.compute = (n_8) -> {
        if (n_8 == 0) {
            return 0.0;
        }
        return extract(cos, n_8 - 1) / (((Number)(n_8)).doubleValue());
};
        return new Pair(sin, cos);
    }

    static double floorf(double x) {
        int y = ((Number)(x)).intValue();
        return ((Number)(y)).doubleValue();
    }

    static String fmtF5(double x) {
        double y_1 = floorf(x * 100000.0 + 0.5) / 100000.0;
        String s_2 = _p(y_1);
        int dot = ((Number)(s_2.indexOf("."))).intValue();
        if (dot == 0 - 1) {
            s_2 = s_2 + ".00000";
        } else {
            int decs = _runeLen(s_2) - dot - 1;
            if (decs > 5) {
                s_2 = _substr(s_2, 0, dot + 6);
            } else {
                while (decs < 5) {
                    s_2 = s_2 + "0";
                    decs = decs + 1;
                }
            }
        }
        return s_2;
    }

    static String padFloat5(double x, int width) {
        String s_3 = String.valueOf(fmtF5(x));
        while (_runeLen(s_3) < width) {
            s_3 = " " + s_3;
        }
        return s_3;
    }

    static String partialSeries(Fps f) {
        String out = "";
        int i_1 = 0;
        while (i_1 < 6) {
            out = out + " " + String.valueOf(padFloat5(extract(f, i_1), 8)) + " ";
            i_1 = i_1 + 1;
        }
        return out;
    }

    static void main() {
        Pair p = sinCos();
        System.out.println("sin:" + String.valueOf(partialSeries(p.sin)));
        System.out.println("cos:" + String.valueOf(partialSeries(p.cos)));
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
