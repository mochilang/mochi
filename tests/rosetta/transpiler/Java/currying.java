public class Main {
    static class Foo {
        int value;
        Foo(int value) {
            this.value = value;
        }
        int Method(int b) {
            return ((Number)(value)).intValue() + b;
        }
        @Override public String toString() {
            return String.format("{'value': %s}", String.valueOf(value));
        }
    }


    static double pow(double base, double exp) {
        double result = 1.0;
        int i = 0;
        while (i < ((Number)(exp)).intValue()) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static java.util.function.Function<Double,Double> PowN(double b) {
        return (e) -> pow(b, e);
    }

    static java.util.function.Function<Double,Double> PowE(double e) {
        return (b) -> pow(b, e);
    }

    static void main() {
        java.util.function.Function<Double,Double> pow2 = PowN(2.0);
        java.util.function.Function<Double,Double> cube = PowE(3.0);
        System.out.println("2^8 = " + String.valueOf(pow2.apply(8.0)));
        System.out.println("4³ = " + String.valueOf(cube.apply(4.0)));
        Foo a = new Foo(2);
        java.util.function.Function<Integer,Integer> fn1 = (b) -> a.Method(b);
        java.util.function.BiFunction<Foo,Integer,Integer> fn2 = (f, b) -> f.Method(b);
        System.out.println("2 + 2 = " + String.valueOf(a.Method(2)));
        System.out.println("2 + 3 = " + String.valueOf(fn1.apply(3)));
        System.out.println("2 + 4 = " + String.valueOf(fn2.apply(a, 4)));
        System.out.println("3 + 5 = " + String.valueOf(fn2.apply(new Foo(3), 5)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
