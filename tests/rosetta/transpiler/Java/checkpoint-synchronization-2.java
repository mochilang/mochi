public class Main {
    static String[] partList;
    static int nAssemblies;

    static String lower(String ch) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        int i = 0;
        while (i < _runeLen(upper)) {
            if ((ch.equals(_substr(upper, i, i + 1)))) {
                return _substr(lower, i, i + 1);
            }
            i = i + 1;
        }
        return ch;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            partList = ((String[])(new String[]{"A", "B", "C", "D"}));
            nAssemblies = 3;
            for (int cycle = 1; cycle < (nAssemblies + 1); cycle++) {
                System.out.println("begin assembly cycle " + _p(cycle));
                String a = "";
                for (String p : partList) {
                    System.out.println(p + " worker begins part");
                    System.out.println(p + " worker completed " + p.toLowerCase());
                    a = a + p.toLowerCase();
                }
                System.out.println(a + " assembled.  cycle " + _p(cycle) + " complete");
            }
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
