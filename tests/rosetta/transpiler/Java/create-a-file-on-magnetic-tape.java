public class Main {

    static Object gzipWriter(Object w) {
        return w;
    }

    static Object tarWriter(Object w) {
        return w;
    }

    static void tarWriteHeader(Object w, java.util.Map<String,Object> hdr) {
    }

    static void tarWrite(Object w, String data) {
    }

    static void main() {
        String filename = "TAPE.FILE";
        String data = "";
        String outfile = "";
        boolean gzipFlag = false;
        Object w = null;
        if (!(outfile.equals(""))) {
            w = null;
        }
        if (gzipFlag) {
            w = gzipWriter(w);
        }
        w = tarWriter(w);
        java.util.Map<String,Object> hdr = ((java.util.Map<String,Object>)(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("Name", filename), java.util.Map.entry("Mode", 432), java.util.Map.entry("Size", _runeLen(data)), java.util.Map.entry("ModTime", _now()), java.util.Map.entry("Typeflag", 0), java.util.Map.entry("Uname", "guest"), java.util.Map.entry("Gname", "guest")))));
        tarWriteHeader(w, hdr);
        tarWrite(w, data);
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
