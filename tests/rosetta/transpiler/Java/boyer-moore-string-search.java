public class Main {

    static int indexOfStr(String h, String n) {
        int hlen = _runeLen(h);
        int nlen = _runeLen(n);
        if (nlen == 0) {
            return 0;
        }
        int i = 0;
        while (i <= hlen - nlen) {
            if ((_substr(h, i, i + nlen).equals(n))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int stringSearchSingle(String h, String n) {
        return indexOfStr(h, n);
    }

    static int[] stringSearch(String h, String n) {
        int[] result = new int[]{};
        int start = 0;
        int hlen_1 = _runeLen(h);
        int nlen_1 = _runeLen(n);
        while (start < hlen_1) {
            int idx = indexOfStr(_substr(h, start, hlen_1), n);
            if (idx >= 0) {
                result = java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(start + idx)).toArray();
                start = start + idx + nlen_1;
            } else {
                break;
            }
        }
        return result;
    }

    static String display(int[] nums) {
        String s = "[";
        int i_1 = 0;
        while (i_1 < nums.length) {
            if (i_1 > 0) {
                s = s + ", ";
            }
            s = s + _p(_geti(nums, i_1));
            i_1 = i_1 + 1;
        }
        s = s + "]";
        return s;
    }

    static void main() {
        String[] texts = new String[]{"GCTAGCTCTACGAGTCTA", "GGCTATAATGCGTA", "there would have been a time for such a word", "needle need noodle needle", "DKnuthusesandprogramsanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguages", "Nearby farms grew an acre of alfalfa on the dairy's behalf, with bales of that alfalfa exchanged for milk."};
        String[] patterns = new String[]{"TCTA", "TAATAAA", "word", "needle", "and", "alfalfa"};
        int i_2 = 0;
        while (i_2 < texts.length) {
            System.out.println("text" + _p(i_2 + 1) + " = " + texts[i_2]);
            i_2 = i_2 + 1;
        }
        System.out.println("");
        int j = 0;
        while (j < texts.length) {
            int[] idxs = stringSearch(texts[j], patterns[j]);
            System.out.println("Found \"" + patterns[j] + "\" in 'text" + _p(j + 1) + "' at indexes " + String.valueOf(display(idxs)));
            j = j + 1;
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
