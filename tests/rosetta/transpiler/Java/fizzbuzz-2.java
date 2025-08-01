public class Main {

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            for (int i = 1; i < 101; i++) {
                java.util.Map<Boolean,java.util.Map<Boolean,String>> m = ((java.util.Map<Boolean,java.util.Map<Boolean,String>>)(new java.util.LinkedHashMap<Boolean, java.util.Map<Boolean,String>>(java.util.Map.ofEntries(java.util.Map.entry(false, ((java.util.Map<Boolean,String>)(new java.util.LinkedHashMap<Boolean, Object>() {{ put(false, String.valueOf(i)); put(true, "Fizz"); }}))), java.util.Map.entry(true, ((java.util.Map<Boolean,String>)(new java.util.LinkedHashMap<Boolean, String>(java.util.Map.ofEntries(java.util.Map.entry(false, "Buzz"), java.util.Map.entry(true, "FizzBuzz"))))))))));
                System.out.println(((String)(((java.util.Map<Boolean,String>)(m).get(Math.floorMod(i, 5) == 0))).get(Math.floorMod(i, 3) == 0)));
            }
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
