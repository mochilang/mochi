public class Main {

    static String strip_and_remove_spaces(String s) {
        int start = 0;
        int end = _runeLen(s) - 1;
        while (start < _runeLen(s) && (s.substring(start, start+1).equals(" "))) {
            start = start + 1;
        }
        while (end >= start && (s.substring(end, end+1).equals(" "))) {
            end = end - 1;
        }
        String res = "";
        int i = start;
        while (i <= end) {
            String ch = s.substring(i, i+1);
            if (!(ch.equals(" "))) {
                res = res + ch;
            }
            i = i + 1;
        }
        return res;
    }

    static boolean check_anagrams(String a, String b) {
        String s1 = a.toLowerCase();
        String s2 = b.toLowerCase();
        s1 = String.valueOf(strip_and_remove_spaces(s1));
        s2 = String.valueOf(strip_and_remove_spaces(s2));
        if (_runeLen(s1) != _runeLen(s2)) {
            return false;
        }
        java.util.Map<String,Integer> count = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        int i_1 = 0;
        while (i_1 < _runeLen(s1)) {
            String c1 = s1.substring(i_1, i_1+1);
            String c2 = s2.substring(i_1, i_1+1);
            if (((Boolean)(count.containsKey(c1)))) {
count.put(c1, (int)(((int)(count).getOrDefault(c1, 0))) + 1);
            } else {
count.put(c1, 1);
            }
            if (((Boolean)(count.containsKey(c2)))) {
count.put(c2, (int)(((int)(count).getOrDefault(c2, 0))) - 1);
            } else {
count.put(c2, -1);
            }
            i_1 = i_1 + 1;
        }
        for (String ch : count.keySet()) {
            if ((int)(((int)(count).getOrDefault(ch, 0))) != 0) {
                return false;
            }
        }
        return true;
    }

    static void print_bool(boolean b) {
        if (((Boolean)(b))) {
            System.out.println(true ? "True" : "False");
        } else {
            System.out.println(false ? "True" : "False");
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            print_bool(check_anagrams("Silent", "Listen"));
            print_bool(check_anagrams("This is a string", "Is this a string"));
            print_bool(check_anagrams("This is    a      string", "Is     this a string"));
            print_bool(check_anagrams("There", "Their"));
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
