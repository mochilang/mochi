public class Main {

    static boolean isEmptyDir(java.util.Map<String,String[]> fs, String name) {
        if (fs.containsKey(name)) {
            return ((String[])(fs).get(name)).length == 0;
        }
        return true;
    }

    static void main() {
        java.util.Map<String,String[]> fs = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
fs.put("/tmp", new String[]{});
fs.put("/var", new String[]{"log"});
        if (isEmptyDir(fs, "/tmp")) {
            System.out.println("/tmp is empty");
        } else {
            System.out.println("/tmp is not empty");
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
