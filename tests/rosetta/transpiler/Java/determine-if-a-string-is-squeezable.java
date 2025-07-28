public class Main {
    static String[] strings = new String[]{"", "\"If I were two-faced, would I be wearing this one?\" --- Abraham Lincoln ", "..1111111111111111111111111111111111111111111111111111111111111117777888", "I never give 'em hell, I just tell the truth, and they think it's hell. ", "                                                   ---  Harry S Truman  ", "The better the 4-wheel drive, the further you'll be from help when ya get stuck!", "headmistressship", "aardvark", "😍😀🙌💃😍😍😍🙌"};
    static String[][] chars = new String[][]{new String[]{" "}, new String[]{"-"}, new String[]{"7"}, new String[]{"."}, new String[]{" ", "-", "r"}, new String[]{"e"}, new String[]{"s"}, new String[]{"a"}, new String[]{"😍"}};
    static int i = 0;

    static String padLeft(int n, int width) {
        String s = String.valueOf(n);
        while (_runeLen(s) < width) {
            s = " " + s;
        }
        return s;
    }

    static String squeeze(String s, String ch) {
        String out = "";
        boolean prev = false;
        int i = 0;
        while (i < _runeLen(s)) {
            String c = _substr(s, i, i + 1);
            if ((c.equals(ch))) {
                if (!prev) {
                    out = out + c;
                    prev = true;
                }
            } else {
                out = out + c;
                prev = false;
            }
            i = i + 1;
        }
        return out;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            while (i < strings.length) {
                int j = 0;
                String s = strings[i];
                while (j < chars[i].length) {
                    String c = chars[i][j];
                    String ss = String.valueOf(squeeze(s, c));
                    System.out.println("specified character = '" + c + "'");
                    System.out.println("original : length = " + String.valueOf(padLeft(_runeLen(s), 2)) + ", string = «««" + s + "»»»");
                    System.out.println("squeezed : length = " + String.valueOf(padLeft(_runeLen(ss), 2)) + ", string = «««" + ss + "»»»");
                    System.out.println("");
                    j = j + 1;
                }
                i = i + 1;
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
}
