public class Main {
    static String stx;
    static String etx;

    static boolean contains(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(ch))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static String[] sortStrings(String[] xs) {
        String[] arr = xs;
        int n = arr.length;
        int i_1 = 0;
        while (i_1 < n) {
            int j = 0;
            while (j < n - 1) {
                if ((arr[j].compareTo(arr[j + 1]) > 0)) {
                    String tmp = arr[j];
arr[j] = arr[j + 1];
arr[j + 1] = tmp;
                }
                j = j + 1;
            }
            i_1 = i_1 + 1;
        }
        return arr;
    }

    static java.util.Map<String,Object> bwt(String s) {
        if (((Boolean)(contains(s, stx))) || ((Boolean)(contains(s, etx)))) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("err", true), java.util.Map.entry("res", "")));
        }
        s = stx + s + etx;
        int le = _runeLen(s);
        String[] table = new String[]{};
        int i_2 = 0;
        while (i_2 < le) {
            String rot = _substr(s, i_2, le) + _substr(s, 0, i_2);
            table = java.util.stream.Stream.concat(java.util.Arrays.stream(table), java.util.stream.Stream.of(rot)).toArray(String[]::new);
            i_2 = i_2 + 1;
        }
        table = sortStrings(table);
        String last = "";
        i_2 = 0;
        while (i_2 < le) {
            last = last + _substr(table[i_2], le - 1, le);
            i_2 = i_2 + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("err", false), java.util.Map.entry("res", last)));
    }

    static String ibwt(String r) {
        int le_1 = _runeLen(r);
        String[] table_1 = new String[]{};
        int i_3 = 0;
        while (i_3 < le_1) {
            table_1 = java.util.stream.Stream.concat(java.util.Arrays.stream(table_1), java.util.stream.Stream.of("")).toArray(String[]::new);
            i_3 = i_3 + 1;
        }
        int n_1 = 0;
        while (n_1 < le_1) {
            i_3 = 0;
            while (i_3 < le_1) {
table_1[i_3] = _substr(r, i_3, i_3 + 1) + table_1[i_3];
                i_3 = i_3 + 1;
            }
            table_1 = sortStrings(table_1);
            n_1 = n_1 + 1;
        }
        i_3 = 0;
        while (i_3 < le_1) {
            if ((_substr(table_1[i_3], le_1 - 1, le_1).equals(etx))) {
                return _substr(table_1[i_3], 1, le_1 - 1);
            }
            i_3 = i_3 + 1;
        }
        return "";
    }

    static String makePrintable(String s) {
        String out = "";
        int i_4 = 0;
        while (i_4 < _runeLen(s)) {
            String ch = _substr(s, i_4, i_4 + 1);
            if ((ch.equals(stx))) {
                out = out + "^";
            } else             if ((ch.equals(etx))) {
                out = out + "|";
            } else {
                out = out + ch;
            }
            i_4 = i_4 + 1;
        }
        return out;
    }

    static void main() {
        String[] examples = new String[]{"banana", "appellee", "dogwood", "TO BE OR NOT TO BE OR WANT TO BE OR NOT?", "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES", "\x02ABC\x03"};
        for (String t : examples) {
            System.out.println(makePrintable(t));
            java.util.Map<String,Object> res = bwt(t);
            if (((boolean) (res.get("err")))) {
                System.out.println(" --> ERROR: String can't contain STX or ETX");
                System.out.println(" -->");
            } else {
                String enc = String.valueOf(((String)(((String) (res.get("res"))))));
                System.out.println(" --> " + String.valueOf(makePrintable(enc)));
                String r = String.valueOf(ibwt(enc));
                System.out.println(" --> " + r);
            }
            System.out.println("");
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            stx = "\x02";
            etx = "\x03";
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
}
