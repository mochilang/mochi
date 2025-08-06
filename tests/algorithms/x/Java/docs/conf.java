public class Main {
    static String pyproject;
    static String project;

    static String parse_project_name(String toml) {
        int i = 0;
        String name = "";
        int n = _runeLen(toml);
        while (i + 4 < n) {
            if ((toml.substring(i, i+1).equals("n")) && (toml.substring(i + 1, i + 1+1).equals("a")) && (toml.substring(i + 2, i + 2+1).equals("m")) && (toml.substring(i + 3, i + 3+1).equals("e"))) {
                i = i + 4;
                while (i < n && !(toml.substring(i, i+1).equals("\""))) {
                    i = i + 1;
                }
                i = i + 1;
                while (i < n && !(toml.substring(i, i+1).equals("\""))) {
                    name = name + toml.substring(i, i+1);
                    i = i + 1;
                }
                return name;
            }
            i = i + 1;
        }
        return name;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            pyproject = "[project]\nname = \"thealgorithms-python\"";
            project = String.valueOf(parse_project_name(pyproject));
            System.out.println(project);
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
