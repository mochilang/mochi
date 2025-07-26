public class Main {
    static String stx = "\u0002";
    static String etx = "\u0003";

    static boolean contains(String s, String ch) {
        int i = 0;
        while (i < s.length()) {
            if ((s.substring(i, i + 1).equals(ch))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static String[] sortStrings(String[] xs) {
        String[] arr = xs;
        int n = arr.length;
        int i = 0;
        while (i < n) {
            int j = 0;
            while (j < n - 1) {
                if ((arr[j].compareTo(arr[j + 1]) > 0)) {
                    String tmp = arr[j];
arr[j] = arr[j + 1];
arr[j + 1] = tmp;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return arr;
    }

    static java.util.Map<String,Object> bwt(String s) {
        if (contains(s, stx) || contains(s, etx)) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("err", true), java.util.Map.entry("res", "")));
        }
        s = stx + s + etx;
        int le = s.length();
        String[] table = new String[]{};
        int i = 0;
        while (i < le) {
            String rot = s.substring(i, le) + s.substring(0, i);
            table = java.util.stream.Stream.concat(java.util.Arrays.stream(table), java.util.stream.Stream.of(rot)).toArray(String[]::new);
            i = i + 1;
        }
        table = sortStrings(table);
        String last = "";
        i = 0;
        while (i < le) {
            last = last + table[i].substring(le - 1, le);
            i = i + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("err", false), java.util.Map.entry("res", last)));
    }

    static String ibwt(String r) {
        int le = r.length();
        String[] table = new String[]{};
        int i = 0;
        while (i < le) {
            table = java.util.stream.Stream.concat(java.util.Arrays.stream(table), java.util.stream.Stream.of("")).toArray(String[]::new);
            i = i + 1;
        }
        int n = 0;
        while (n < le) {
            i = 0;
            while (i < le) {
table[i] = r.substring(i, i + 1) + table[i];
                i = i + 1;
            }
            table = sortStrings(table);
            n = n + 1;
        }
        i = 0;
        while (i < le) {
            if ((table[i].substring(le - 1, le).equals(etx))) {
                return table[i].substring(1, le - 1);
            }
            i = i + 1;
        }
        return "";
    }

    static String makePrintable(String s) {
        String out = "";
        int i = 0;
        while (i < s.length()) {
            String ch = s.substring(i, i + 1);
            if ((ch.equals(stx))) {
                out = out + "^";
            } else             if ((ch.equals(etx))) {
                out = out + "|";
            } else {
                out = out + ch;
            }
            i = i + 1;
        }
        return out;
    }

    static void main() {
        String[] examples = new String[]{"banana", "appellee", "dogwood", "TO BE OR NOT TO BE OR WANT TO BE OR NOT?", "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES", "\u0002ABC\u0003"};
        for (String t : examples) {
            System.out.println(makePrintable(t));
            java.util.Map<String,Object> res = bwt(t);
            if (((boolean)res.getOrDefault("err", false))) {
                System.out.println(" --> ERROR: String can't contain STX or ETX");
                System.out.println(" -->");
            } else {
                String enc = ((String)res.get("res"));
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
}
