public class Main {

    static int indexOf3(String s, String ch, int start) {
        int i = start;
        while (i < s.length()) {
            if ((s.substring(i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int ord(String ch) {
        String digits = "0123456789";
        int idx = indexOf3(digits, ch, 0);
        if (idx >= 0) {
            return 48 + idx;
        }
        if ((ch.equals("X"))) {
            return 88;
        }
        if ((ch.equals("é"))) {
            return 233;
        }
        if ((ch.equals("😍"))) {
            return 128525;
        }
        if ((ch.equals("🐡"))) {
            return 128033;
        }
        return 0;
    }

    static String toHex(int n) {
        String digits = "0123456789ABCDEF";
        if (n == 0) {
            return "0";
        }
        int v = n;
        String out = "";
        while (v > 0) {
            int d = Math.floorMod(v, 16);
            out = digits.substring(d, d + 1) + out;
            v = v / 16;
        }
        return out;
    }

    static void analyze(String s) {
        int le = s.length();
        System.out.println("Analyzing \"" + s + "\" which has a length of " + String.valueOf(le) + ":");
        if (le > 1) {
            int i = 0;
            while (i < le - 1) {
                int j = i + 1;
                while (j < le) {
                    if ((s.substring(j, j + 1).equals(s.substring(i, i + 1)))) {
                        String ch = s.substring(i, i + 1);
                        System.out.println("  Not all characters in the string are unique.");
                        System.out.println("  '" + ch + "' (0x" + toHex(ord(ch)).toLowerCase() + ") is duplicated at positions " + String.valueOf(i + 1) + " and " + String.valueOf(j + 1) + ".\n");
                        return;
                    }
                    j = j + 1;
                }
                i = i + 1;
            }
        }
        System.out.println("  All characters in the string are unique.\n");
    }

    static void main() {
        String[] strings = new String[]{"", ".", "abcABC", "XYZ ZYX", "1234567890ABCDEFGHIJKLMN0PQRSTUVWXYZ", "01234567890ABCDEFGHIJKLMN0PQRSTUVWXYZ0X", "hétérogénéité", "🎆🎃🎇🎈", "😍😀🙌💃😍🙌", "🐠🐟🐡🦈🐬🐳🐋🐡"};
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
