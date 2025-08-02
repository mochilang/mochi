public class Main {

    static boolean parseBool(String s) {
        String l = s.toLowerCase();
        if ((l.equals("1")) || (l.equals("t")) || (l.equals("true")) || (l.equals("yes")) || (l.equals("y"))) {
            return true;
        }
        return false;
    }

    static void main() {
        boolean n = true;
        System.out.println(n ? "True" : "False");
        System.out.println("bool");
        n = !n;
        System.out.println(n ? "True" : "False");
        int x = 5;
        int y = 8;
        System.out.println("x == y:" + " " + String.valueOf(x == y ? 1 : 0));
        System.out.println("x < y:" + " " + String.valueOf(x < y ? 1 : 0));
        System.out.println("\nConvert String into Boolean Data type\n");
        String str1 = "japan";
        System.out.println("Before :" + " " + "string");
        boolean bolStr = parseBool(str1);
        System.out.println("After :" + " " + "bool");
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
