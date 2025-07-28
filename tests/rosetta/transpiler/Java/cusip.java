public class Main {
    static String[] candidates = new String[]{"037833100", "17275R102", "38259P508", "594918104", "68389X106", "68389X105"};

    static int ord(String ch) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        if ((ch.compareTo("0") >= 0) && (ch.compareTo("9") <= 0)) {
            return Integer.parseInt(ch, 10) + 48;
        }
        int idx = ((Number)(upper.indexOf(ch))).intValue();
        if (idx >= 0) {
            return 65 + idx;
        }
        return 0;
    }

    static boolean isCusip(String s) {
        if (_runeLen(s) != 9) {
            return false;
        }
        int sum = 0;
        int i = 0;
        while (i < 8) {
            String c = s.substring(i, i + 1);
            int v = 0;
            if ((c.compareTo("0") >= 0) && (c.compareTo("9") <= 0)) {
                v = Integer.parseInt(c, 10);
            } else             if ((c.compareTo("A") >= 0) && (c.compareTo("Z") <= 0)) {
                v = ord(c) - 55;
            } else             if ((c.equals("*"))) {
                v = 36;
            } else             if ((c.equals("@"))) {
                v = 37;
            } else             if ((c.equals("#"))) {
                v = 38;
            } else {
                return false;
            }
            if (Math.floorMod(i, 2) == 1) {
                v = v * 2;
            }
            sum = sum + v / 10 + Math.floorMod(v, 10);
            i = i + 1;
        }
        return Integer.parseInt(s.substring(8, 9), 10) == Math.floorMod((10 - (Math.floorMod(sum, 10))), 10);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            for (String cand : candidates) {
                String b = "incorrect";
                if (isCusip(cand)) {
                    b = "correct";
                }
                System.out.println(cand + " -> " + b);
            }
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
