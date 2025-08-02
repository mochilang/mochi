public class Main {
    static String[] extensions;

    static boolean endsWith(String s, String suf) {
        if (_runeLen(s) < _runeLen(suf)) {
            return false;
        }
        return (_substr(s, _runeLen(s) - _runeLen(suf), _runeLen(s)).equals(suf));
    }

    static int lastIndexOf(String s, String sub) {
        int idx = 0 - 1;
        int i = 0;
        while (i <= _runeLen(s) - _runeLen(sub)) {
            if ((_substr(s, i, i + _runeLen(sub)).equals(sub))) {
                idx = i;
            }
            i = i + 1;
        }
        return idx;
    }

    static Object[] fileExtInList(String filename) {
        String fl = filename.toLowerCase();
        for (String ext : extensions) {
            String ext2 = "." + ext.toLowerCase();
            if (((Boolean)(endsWith(fl, ext2)))) {
                return new Object[]{true, ext};
            }
        }
        int idx_1 = lastIndexOf(filename, ".");
        if (idx_1 != 0 - 1) {
            String t = _substr(filename, idx_1 + 1, _runeLen(filename));
            if (!(t.equals(""))) {
                return new Object[]{false, t};
            }
            return new Object[]{false, "<empty>"};
        }
        return new Object[]{false, "<none>"};
    }

    static String pad(String s, int w) {
        String t_1 = s;
        while (_runeLen(t_1) < w) {
            t_1 = t_1 + " ";
        }
        return t_1;
    }

    static void main() {
        System.out.println("The listed extensions are:");
        System.out.println(java.util.Arrays.toString(extensions));
        String[] tests = new String[]{"MyData.a##", "MyData.tar.Gz", "MyData.gzip", "MyData.7z.backup", "MyData...", "MyData", "MyData_v1.0.tar.bz2", "MyData_v1.0.bz2"};
        for (String t : tests) {
            Object[] res = fileExtInList(t);
            boolean ok = ((Boolean)(((boolean)(res[0]))));
            String ext = ((String)(res[1]));
            System.out.println(String.valueOf(pad(t, 20)) + " => " + _p(ok) + "  (extension = " + ext + ")");
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            extensions = new String[]{"zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2"};
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
