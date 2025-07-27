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
        java.util.Map<String,Object> hdr = new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("Name", filename), java.util.Map.entry("Mode", 432), java.util.Map.entry("Size", data.length()), java.util.Map.entry("ModTime", _now()), java.util.Map.entry("Typeflag", 0), java.util.Map.entry("Uname", "guest"), java.util.Map.entry("Gname", "guest")));
        tarWriteHeader(w, hdr);
        tarWrite(w, data);
    }
    public static void main(String[] args) {
        main();
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
}
