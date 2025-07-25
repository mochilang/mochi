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

    static boolean deranged(String a, String b) {
        if (a.length() != b.length()) {
            return false;
        }
        int i = 0;
        while (i < a.length()) {
            if ((a.substring(i, i + 1).equals(b.substring(i, i + 1)))) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static void main() {
        String[] words = new String[]{"constitutionalism", "misconstitutional"};
        java.util.Map<String,String[]> m = new java.util.LinkedHashMap<String, String[]>();
        int bestLen = 0;
        String w1 = "";
        String w2 = "";
        for (var w : words) {
            if (w.length() <= bestLen) {
                continue;
            }
            String k = String.valueOf(sortRunes(w));
            if (!(Boolean)(m.containsKey(k))) {
m.put(k, new String[]{w});
                continue;
            }
            for (var c : ((String[])m.get(k))) {
                if (deranged(w, c)) {
                    bestLen = w.length();
                    w1 = c;
                    w2 = w;
                    break;
                }
            }
m.put(k, java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])m.get(k))), java.util.stream.Stream.of(w)).toArray(String[]::new));
        }
        System.out.println(String.valueOf(String.valueOf(String.valueOf(w1 + " ") + w2) + " : Length ") + String.valueOf(bestLen));
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
