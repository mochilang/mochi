public class Main {
    static String[] compassPoint;
    static double[] headings;
    static int i_2;

    static String padLeft(String s, int w) {
        String res = "";
        int n = w - _runeLen(s);
        while (n > 0) {
            res = res + " ";
            n = n - 1;
        }
        return res + s;
    }

    static String padRight(String s, int w) {
        String out = s;
        int i = _runeLen(s);
        while (i < w) {
            out = out + " ";
            i = i + 1;
        }
        return out;
    }

    static int indexOf(String s, String ch) {
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            if ((_substr(s, i_1, i_1 + 1).equals(ch))) {
                return i_1;
            }
            i_1 = i_1 + 1;
        }
        return -1;
    }

    static String format2(double f) {
        String s = _p(f);
        int idx = ((Number)(s.indexOf("."))).intValue();
        if (idx < 0) {
            s = s + ".00";
        } else {
            int need = idx + 3;
            if (_runeLen(s) > need) {
                s = _substr(s, 0, need);
            } else {
                while (_runeLen(s) < need) {
                    s = s + "0";
                }
            }
        }
        return s;
    }

    static int cpx(double h) {
        int x = ((Number)(((h / 11.25) + 0.5))).intValue();
        x = Math.floorMod(x, 32);
        if (x < 0) {
            x = x + 32;
        }
        return x;
    }

    static String degrees2compasspoint(double h) {
        return compassPoint[cpx(h)];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            compassPoint = new String[]{"North", "North by east", "North-northeast", "Northeast by north", "Northeast", "Northeast by east", "East-northeast", "East by north", "East", "East by south", "East-southeast", "Southeast by east", "Southeast", "Southeast by south", "South-southeast", "South by east", "South", "South by west", "South-southwest", "Southwest by south", "Southwest", "Southwest by west", "West-southwest", "West by south", "West", "West by north", "West-northwest", "Northwest by west", "Northwest", "Northwest by north", "North-northwest", "North by west"};
            headings = new double[]{0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38};
            System.out.println("Index  Compass point         Degree");
            i_2 = 0;
            while (i_2 < headings.length) {
                double h = headings[i_2];
                int idx_1 = Math.floorMod(i_2, 32) + 1;
                String cp = String.valueOf(degrees2compasspoint(h));
                System.out.println(String.valueOf(padLeft(_p(idx_1), 4)) + "   " + String.valueOf(padRight(cp, 19)) + " " + String.valueOf(format2(h)) + "°");
                i_2 = i_2 + 1;
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
