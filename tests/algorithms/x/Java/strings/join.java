public class Main {

    static String join(String separator, String[] separated) {
        String joined = "";
        int last_index = separated.length - 1;
        int i = 0;
        while (i < separated.length) {
            joined = joined + separated[i];
            if (i < last_index) {
                joined = joined + separator;
            }
            i = i + 1;
        }
        return joined;
    }

    static void main() {
        System.out.println(join("", ((String[])(new String[]{"a", "b", "c", "d"}))));
        System.out.println(join("#", ((String[])(new String[]{"a", "b", "c", "d"}))));
        System.out.println(join("#", ((String[])(new String[]{"a"}))));
        System.out.println(join(" ", ((String[])(new String[]{"You", "are", "amazing!"}))));
        System.out.println(join(",", ((String[])(new String[]{"", "", ""}))));
        System.out.println(join("-", ((String[])(new String[]{"apple", "banana", "cherry"}))));
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
