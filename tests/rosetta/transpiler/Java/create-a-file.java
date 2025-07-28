public class Main {

    static void createFile(java.util.Map<String,Boolean> fs, String fn) {
        if (fs.containsKey(fn)) {
            System.out.println("open " + fn + ": file exists");
        } else {
fs.put(fn, false);
            System.out.println("file " + fn + " created!");
        }
    }

    static void createDir(java.util.Map<String,Boolean> fs, String dn) {
        if (fs.containsKey(dn)) {
            System.out.println("mkdir " + dn + ": file exists");
        } else {
fs.put(dn, true);
            System.out.println("directory " + dn + " created!");
        }
    }

    static void main() {
        java.util.Map<String,Boolean> fs = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
fs.put("docs", true);
        createFile(fs, "input.txt");
        createFile(fs, "/input.txt");
        createDir(fs, "docs");
        createDir(fs, "/docs");
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
