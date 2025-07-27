public class Main {

    static java.util.Map<String,Integer> newTerm(int a, int b) {
        return new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", a), java.util.Map.entry("b", b)));
    }

    static java.util.Map<String,Integer>[] cfSqrt2(int nTerms) {
        java.util.Map<String,Integer>[] f = (java.util.Map<String,Integer>[])new java.util.Map[]{};
        int n = 0;
        while (n < nTerms) {
            f = appendObj(f, newTerm(2, 1));
            n = n + 1;
        }
        if (nTerms > 0) {
f[0].put("a", 1);
        }
        return f;
    }

    static java.util.Map<String,Integer>[] cfNap(int nTerms) {
        java.util.Map<String,Integer>[] f = (java.util.Map<String,Integer>[])new java.util.Map[]{};
        int n = 0;
        while (n < nTerms) {
            f = appendObj(f, newTerm(n, n - 1));
            n = n + 1;
        }
        if (nTerms > 0) {
f[0].put("a", 2);
        }
        if (nTerms > 1) {
f[1].put("b", 1);
        }
        return f;
    }

    static java.util.Map<String,Integer>[] cfPi(int nTerms) {
        java.util.Map<String,Integer>[] f = (java.util.Map<String,Integer>[])new java.util.Map[]{};
        int n = 0;
        while (n < nTerms) {
            int g = 2 * n - 1;
            f = appendObj(f, newTerm(6, g * g));
            n = n + 1;
        }
        if (nTerms > 0) {
f[0].put("a", 3);
        }
        return f;
    }

    static double real(java.util.Map<String,Integer>[] f) {
        double r = 0.0;
        int i = f.length - 1;
        while (i > 0) {
            r = (((double)f[i].getOrDefault("b", 0))) / ((((double)f[i].getOrDefault("a", 0))) + r);
            i = i - 1;
        }
        if (f.length > 0) {
            r = r + (((double)f[0].getOrDefault("a", 0)));
        }
        return r;
    }

    static void main() {
        System.out.println("sqrt2: " + String.valueOf(real(cfSqrt2(20))));
        System.out.println("nap:   " + String.valueOf(real(cfNap(20))));
        System.out.println("pi:    " + String.valueOf(real(cfPi(20))));
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
