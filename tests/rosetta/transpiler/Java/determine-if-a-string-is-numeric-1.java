public class Main {

    static boolean isNumeric(String s) {
        if ((s.equals("NaN"))) {
            return true;
        }
        int i = 0;
        if (s.length() == 0) {
            return false;
        }
        if ((s.substring(0, 0+1).equals("+")) || (s.substring(0, 0+1).equals("-"))) {
            if (s.length() == 1) {
                return false;
            }
            i = 1;
        }
        boolean digits = false;
        boolean dot = false;
        while (i < s.length()) {
            String ch = s.substring(i, i+1);
            if ((ch.compareTo("0") >= 0) && (ch.compareTo("9") <= 0)) {
                digits = true;
                i = i + 1;
            } else             if ((ch.equals(".")) && dot == false) {
                dot = true;
                i = i + 1;
            } else             if (((ch.equals("e")) || (ch.equals("E"))) && digits) {
                i = i + 1;
                if (i < s.length() && ((s.substring(i, i+1).equals("+")) || (s.substring(i, i+1).equals("-")))) {
                    i = i + 1;
                }
                boolean ed = false;
                while (i < s.length() && (s.substring(i, i+1).compareTo("0") >= 0) && (s.substring(i, i+1).compareTo("9") <= 0)) {
                    ed = true;
                    i = i + 1;
                }
                return ed && i == s.length();
            } else {
                return false;
            }
        }
        return digits;
    }

    static void main() {
        System.out.println("Are these strings numeric?");
        String[] strs = new String[]{"1", "3.14", "-100", "1e2", "NaN", "rose"};
        for (String s : strs) {
            System.out.println("  " + s + " -> " + String.valueOf(isNumeric(s)));
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
