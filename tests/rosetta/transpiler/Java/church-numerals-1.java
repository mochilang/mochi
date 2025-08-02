public class Main {
    static java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> z;
    static java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> three;
    static java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> four;

    static java.util.function.Function<Object,Object> zero(java.util.function.Function<Object,Object> f) {
        return (x) -> x;
    }

    static java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> succ(java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> c) {
        return (f) -> (x_1) -> f.apply(c.apply(f).apply(x_1));
    }

    static java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> add(java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> c, java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> d) {
        return (f_1) -> (x_2) -> c.apply(f_1).apply(d.apply(f_1).apply(x_2));
    }

    static java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> mul(java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> c, java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> d) {
        return (f_2) -> (x_3) -> c.apply(d.apply(f_2)).apply(x_3);
    }

    static java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> pow(java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> c, java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> d) {
        int di = toInt(d);
        java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> prod = c;
        int i = 1;
        while (i < di) {
            prod = mul(prod, c);
            i = i + 1;
        }
        return prod;
    }

    static Object incr(Object i) {
        return (((Number)(i)).intValue()) + 1;
    }

    static java.util.function.Function<Object,Integer> toInt(java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> c) {
        return ((Number)(c.apply(incr).apply(0))).intValue();
    }

    static java.util.function.Function<java.util.function.Function<Object,Object>,java.util.function.Function<Object,Object>> intToChurch(java.util.function.Function<Object,Integer> i) {
        if (i == 0) {
            return zero;
        }
        return succ(intToChurch(i - 1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            z = zero;
            three = succ(succ(succ(z)));
            four = succ(three);
            System.out.println("three        -> " + _p(toInt(three)));
            System.out.println("four         -> " + _p(toInt(four)));
            System.out.println("three + four -> " + _p(toInt(add(three, four))));
            System.out.println("three * four -> " + _p(toInt(mul(three, four))));
            System.out.println("three ^ four -> " + _p(toInt(pow(three, four))));
            System.out.println("four ^ three -> " + _p(toInt(pow(four, three))));
            System.out.println("5 -> five    -> " + _p(toInt(intToChurch(5))));
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
