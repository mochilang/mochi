public class Main {

    static String[][] findDuplicates(java.util.Map<String,String> fs, String[] paths) {
        java.util.Map<String,String> seen = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>()));
        String[][] dups = new String[][]{};
        for (String path : paths) {
            String content = ((String)(fs).get(path));
            if (seen.containsKey(content)) {
                dups = appendObj(dups, new Object[]{((String)(seen).get(content)), path});
            } else {
seen.put(content, path);
            }
        }
        return dups;
    }

    static void main() {
        java.util.Map<String,String> fs = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("a.txt", "hello"), java.util.Map.entry("b.txt", "world"), java.util.Map.entry("c.txt", "hello"), java.util.Map.entry("d.txt", "foo"), java.util.Map.entry("e.txt", "world")))));
        String[] paths = new String[]{"a.txt", "b.txt", "c.txt", "d.txt", "e.txt"};
        String[][] dups_1 = findDuplicates(fs, paths);
        for (String[] pair : dups_1) {
            System.out.println(pair[0] + " <==> " + pair[1]);
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
