public class Main {

    static String sortRunes(String s) {
        String[] arr = new String[]{};
        int i = 0;
        while (i < s.length()) {
            arr = java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(s.substring(i, i + 1))).toArray(String[]::new);
            i = i + 1;
        }
        int n = arr.length;
        int m = 0;
        while (m < n) {
            int j = 0;
            while (j < n - 1) {
                if ((arr[j].compareTo(arr[j + 1]) > 0)) {
                    String tmp = arr[j];
arr[j] = arr[j + 1];
arr[j + 1] = tmp;
                }
                j = j + 1;
            }
            m = m + 1;
        }
        String out = "";
        i = 0;
        while (i < n) {
            out = String.valueOf(out + arr[i]);
            i = i + 1;
        }
        return out;
    }

    static String[] sortStrings(String[] xs) {
        String[] res = new String[]{};
        String[] tmp = xs;
        while (tmp.length > 0) {
            String min = tmp[0];
            int idx = 0;
            int i = 1;
            while (i < tmp.length) {
                if ((tmp[i].compareTo(min) < 0)) {
                    min = tmp[i];
                    idx = i;
                }
                i = i + 1;
            }
            res = java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(min)).toArray(String[]::new);
            String[] out = new String[]{};
            int j = 0;
            while (j < tmp.length) {
                if (j != idx) {
                    out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(tmp[j])).toArray(String[]::new);
                }
                j = j + 1;
            }
            tmp = out;
        }
        return res;
    }

    static void main() {
        String[] words = new String[]{"abel", "able", "bale", "bela", "elba", "alger", "glare", "lager", "large", "regal", "angel", "angle", "galen", "glean", "lange", "caret", "carte", "cater", "crate", "trace", "elan", "lane", "lean", "lena", "neal", "evil", "levi", "live", "veil", "vile"};
        java.util.Map<String,String[]> groups = new java.util.LinkedHashMap<String, String[]>();
        int maxLen = 0;
        for (var w : words) {
            String k = String.valueOf(sortRunes(w));
            if (!(Boolean)(groups.containsKey(k))) {
groups.put(k, new String[]{w});
            } else {
groups.put(k, java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])groups.get(k))), java.util.stream.Stream.of(w)).toArray(String[]::new));
            }
            if (((String[])groups.get(k)).length > maxLen) {
                maxLen = ((String[])groups.get(k)).length;
            }
        }
        java.util.Map<String,Boolean> printed = new java.util.LinkedHashMap<String, Boolean>();
        for (var w : words) {
            String k = String.valueOf(sortRunes(w));
            if (((String[])groups.get(k)).length == maxLen) {
                if (!(Boolean)(printed.containsKey(k))) {
                    String[] g = sortStrings((String[])(((String[])groups.get(k))));
                    String line = String.valueOf("[" + String.valueOf(g[0]));
                    int i = 1;
                    while (i < g.length) {
                        line = String.valueOf(line + " " + g[i]);
                        i = i + 1;
                    }
                    line = String.valueOf(line + "]");
                    System.out.println(line);
printed.put(k, true);
                }
            }
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
