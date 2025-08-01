public class Main {

    static String[] splitPath(String p) {
        String[] parts = new String[]{};
        String cur = "";
        int i = 0;
        while (i < _runeLen(p)) {
            if ((_substr(p, i, i + 1).equals("/"))) {
                if (!(cur.equals(""))) {
                    parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                    cur = "";
                }
            } else {
                cur = cur + _substr(p, i, i + 1);
            }
            i = i + 1;
        }
        if (!(cur.equals(""))) {
            parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        }
        return parts;
    }

    static String joinPath(String[] parts) {
        String s = "";
        int i_1 = 0;
        while (i_1 < parts.length) {
            s = s + "/" + parts[i_1];
            i_1 = i_1 + 1;
        }
        return s;
    }

    static String commonPrefix(String[] paths) {
        if (paths.length == 0) {
            return "";
        }
        String[] base = splitPath(paths[0]);
        int i_2 = 0;
        String[] prefix = new String[]{};
        while (i_2 < base.length) {
            String comp = base[i_2];
            boolean ok = true;
            for (String p : paths) {
                String[] parts_1 = splitPath(p);
                if (i_2 >= parts_1.length || !(parts_1[i_2].equals(comp))) {
                    ok = false;
                    break;
                }
            }
            if (ok) {
                prefix = java.util.stream.Stream.concat(java.util.Arrays.stream(prefix), java.util.stream.Stream.of(comp)).toArray(String[]::new);
            } else {
                break;
            }
            i_2 = i_2 + 1;
        }
        return joinPath(prefix);
    }

    static void main() {
        String[] paths = new String[]{"/home/user1/tmp/coverage/test", "/home/user1/tmp/covert/operator", "/home/user1/tmp/coven/members", "/home//user1/tmp/coventry", "/home/user1/././tmp/covertly/foo", "/home/bob/../user1/tmp/coved/bar"};
        String c = String.valueOf(commonPrefix(paths));
        if ((c.equals(""))) {
            System.out.println("No common path");
        } else {
            System.out.println("Common path: " + c);
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
