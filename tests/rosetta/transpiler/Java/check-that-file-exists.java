public class Main {

    static void printStat(java.util.Map<String,Boolean> fs, String path) {
        if (((Boolean)(fs.containsKey(path)))) {
            if (((boolean)(fs).getOrDefault(path, false))) {
                System.out.println(path + " is a directory");
            } else {
                System.out.println(path + " is a file");
            }
        } else {
            System.out.println("stat " + path + ": no such file or directory");
        }
    }

    static void main() {
        java.util.Map<String,Boolean> fs = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
fs.put("docs", true);
        for (String p : new String[]{"input.txt", "/input.txt", "docs", "/docs"}) {
            printStat(fs, p);
        }
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
