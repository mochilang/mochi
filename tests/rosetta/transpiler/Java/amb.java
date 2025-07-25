public class Main {

    static boolean amb(String[][] wordsets, String[] res, int idx) {
        if (idx == wordsets.length) {
            return true;
        }
        String prev = "";
        if (idx > 0) {
            prev = String.valueOf(res[idx - 1]);
        }
        int i = 0;
        while (i < wordsets[idx].length) {
            String w = String.valueOf(wordsets[idx][i]);
            if ((idx == 0 || prev.substring(prev.length() - 1, prev.length()).equals(w.substring(0, 1)))) {
res[idx] = w;
                if (amb(wordsets, res, idx + 1)) {
                    return true;
                }
            }
            i = i + 1;
        }
        return false;
    }

    static void main() {
        String[][] wordset = new String[][]{new String[]{"the", "that", "a"}, new String[]{"frog", "elephant", "thing"}, new String[]{"walked", "treaded", "grows"}, new String[]{"slowly", "quickly"}};
        String[] res = new String[]{};
        int i = 0;
        while (i < wordset.length) {
            res = java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of("")).toArray(String[]::new);
            i = i + 1;
        }
        if (amb(wordset, res, 0)) {
            String out = String.valueOf("[" + String.valueOf(res[0]));
            int j = 1;
            while (j < res.length) {
                out = String.valueOf(out + " " + res[j]);
                j = j + 1;
            }
            out = String.valueOf(out + "]");
            System.out.println(out);
        } else {
            System.out.println("No amb found");
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
