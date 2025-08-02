public class Main {

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((s.substring(i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String rotate(String s, int n) {
        return s.substring(n, _runeLen(s)) + s.substring(0, n);
    }

    static String scrambleLeft(String s) {
        return s.substring(0, 1) + s.substring(2, 14) + s.substring(1, 2) + s.substring(14, _runeLen(s));
    }

    static String scrambleRight(String s) {
        return s.substring(1, 3) + s.substring(4, 15) + s.substring(3, 4) + s.substring(15, _runeLen(s)) + s.substring(0, 1);
    }

    static String chao(String text, boolean encode) {
        String left = "HXUCZVAMDSLKPEFJRIGTWOBNYQ";
        String right = "PTLNBQDEOYSFAVZKGJRIHWXUMC";
        String out = "";
        int i_1 = 0;
        while (i_1 < _runeLen(text)) {
            String ch = text.substring(i_1, i_1 + 1);
            int idx = 0;
            if (((Boolean)(encode))) {
                idx = ((Number)(right.indexOf(ch))).intValue();
                out = out + left.substring(idx, idx + 1);
            } else {
                idx = ((Number)(left.indexOf(ch))).intValue();
                out = out + right.substring(idx, idx + 1);
            }
            left = String.valueOf(rotate(left, idx));
            right = String.valueOf(rotate(right, idx));
            left = String.valueOf(scrambleLeft(left));
            right = String.valueOf(scrambleRight(right));
            i_1 = i_1 + 1;
        }
        return out;
    }

    static void main() {
        String plain = "WELLDONEISBETTERTHANWELLSAID";
        String cipher = String.valueOf(chao(plain, true));
        System.out.println(plain);
        System.out.println(cipher);
        System.out.println(chao(cipher, false));
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
}
