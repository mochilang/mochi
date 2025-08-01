public class Main {

    static void printSize(java.util.Map<String,Integer> fs, String path) {
        if (fs.containsKey(path)) {
            System.out.println(String.valueOf(((int)(fs).getOrDefault(path, 0))));
        } else {
            System.out.println("stat " + path + ": no such file or directory");
        }
    }

    static void main() {
        java.util.Map<String,Integer> fs = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
fs.put("input.txt", 123);
        printSize(fs, "input.txt");
        printSize(fs, "/input.txt");
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
