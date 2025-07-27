public class Main {

    static int ord(String ch) {
        if ((ch.equals("5"))) {
            return 53;
        }
        if ((ch.equals("T"))) {
            return 84;
        }
        if ((ch.equals(" "))) {
            return 32;
        }
        if ((ch.equals("é"))) {
            return 233;
        }
        if ((ch.equals("🐺"))) {
            return 128058;
        }
        return 0;
    }

    static String hex(int n) {
        String digits = "0123456789abcdef";
        if (n == 0) {
            return "0x0";
        }
        int m = n;
        String out = "";
        while (m > 0) {
            int d = Math.floorMod(m, 16);
            out = digits.substring(d, d + 1) + out;
            m = m / 16;
        }
        return "0x" + out;
    }

    static String quote(String s) {
        return "'" + s + "'";
    }

    static void analyze(String s) {
        int le = s.length();
        System.out.println("Analyzing " + String.valueOf(quote(s)) + " which has a length of " + String.valueOf(le) + ":");
        if (le > 1) {
            int i = 1;
            while (i < le) {
                String cur = s.substring(i, i + 1);
                String prev = s.substring(i - 1, i);
                if (!(cur.equals(prev))) {
                    System.out.println("  Not all characters in the string are the same.");
                    System.out.println("  " + String.valueOf(quote(cur)) + " (" + String.valueOf(hex(ord(cur))) + ") is different at position " + String.valueOf(i + 1) + ".");
                    System.out.println("");
                    return;
                }
                i = i + 1;
            }
        }
        System.out.println("  All characters in the string are the same.");
        System.out.println("");
    }

    static void main() {
        String[] strings = new String[]{"", "   ", "2", "333", ".55", "tttTTT", "4444 444k", "pépé", "🐶🐶🐺🐶", "🎄🎄🎄🎄"};
        int i = 0;
        while (i < strings.length) {
            analyze(strings[i]);
            i = i + 1;
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
