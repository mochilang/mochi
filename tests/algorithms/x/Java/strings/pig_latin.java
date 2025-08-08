public class Main {
    static String VOWELS;

    static String strip(String s) {
        int start = 0;
        int end = _runeLen(s);
        while (start < end && (_substr(s, start, start + 1).equals(" "))) {
            start = start + 1;
        }
        while (end > start && (_substr(s, end - 1, end).equals(" "))) {
            end = end - 1;
        }
        return _substr(s, start, end);
    }

    static boolean is_vowel(String c) {
        int i = 0;
        while (i < _runeLen(VOWELS)) {
            if ((c.equals(_substr(VOWELS, i, i + 1)))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static String pig_latin(String word) {
        String trimmed = String.valueOf(strip(word));
        if (_runeLen(trimmed) == 0) {
            return "";
        }
        String w = trimmed.toLowerCase();
        String first = _substr(w, 0, 1);
        if (((Boolean)(is_vowel(first)))) {
            return w + "way";
        }
        int i_1 = 0;
        while (i_1 < _runeLen(w)) {
            String ch = _substr(w, i_1, i_1 + 1);
            if (((Boolean)(is_vowel(ch)))) {
                break;
            }
            i_1 = i_1 + 1;
        }
        return _substr(w, i_1, _runeLen(w)) + _substr(w, 0, i_1) + "ay";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            VOWELS = "aeiou";
            System.out.println("pig_latin('friends') = " + String.valueOf(pig_latin("friends")));
            System.out.println("pig_latin('smile') = " + String.valueOf(pig_latin("smile")));
            System.out.println("pig_latin('eat') = " + String.valueOf(pig_latin("eat")));
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
