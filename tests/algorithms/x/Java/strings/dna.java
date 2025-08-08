public class Main {

    static boolean is_valid(String strand) {
        int i = 0;
        while (i < _runeLen(strand)) {
            String ch = _substr(strand, i, i + 1);
            if (!(ch.equals("A")) && !(ch.equals("T")) && !(ch.equals("C")) && !(ch.equals("G"))) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static String dna(String strand) {
        if (!(Boolean)is_valid(strand)) {
            System.out.println("ValueError: Invalid Strand");
            return "";
        }
        String result = "";
        int i_1 = 0;
        while (i_1 < _runeLen(strand)) {
            String ch_1 = _substr(strand, i_1, i_1 + 1);
            if ((ch_1.equals("A"))) {
                result = result + "T";
            } else             if ((ch_1.equals("T"))) {
                result = result + "A";
            } else             if ((ch_1.equals("C"))) {
                result = result + "G";
            } else {
                result = result + "C";
            }
            i_1 = i_1 + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(dna("GCTA"));
            System.out.println(dna("ATGC"));
            System.out.println(dna("CTGA"));
            System.out.println(dna("GFGG"));
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
