public class Main {
    static String name;
    static String prefix;

    static boolean hasPrefix(String s, String p) {
        if (_runeLen(p) > _runeLen(s)) {
            return false;
        }
        return (_substr(s, 0, _runeLen(p)).equals(p));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            name = "SHELL";
            prefix = name + "=";
            for (var v : _environ()) {
                if (hasPrefix((String)(v), prefix)) {
                    System.out.println(name + " has value " + _substr(v, _runeLen(prefix), String.valueOf(v).length()));
                    return;
                }
            }
            System.out.println(name + " not found");
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

    static String[] _environ() {
        java.util.Map<String,String> env = System.getenv();
        String[] out = new String[env.size()];
        int i = 0;
        for (java.util.Map.Entry<String,String> e : env.entrySet()) { out[i++] = e.getKey()+"="+e.getValue(); }
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
