public class Main {

    static Object[] collapse(String s) {
        int i = 0;
        String prev = "";
        String res = "";
        int orig = s.length();
        while (i < s.length()) {
            String ch = s.substring(i, i + 1);
            if (!(ch.equals(prev))) {
                res = res + ch;
                prev = ch;
            }
            i = i + 1;
        }
        return new String[]{res, orig, res.length()};
    }

    static void main() {
        String[] strings = new String[]{"", "\"If I were two-faced, would I be wearing this one?\" --- Abraham Lincoln ", "..111111111111111111111111111111111111111111111111111111111111111777888", "I never give 'em hell, I just tell the truth, and they think it's hell. ", "                                                   ---  Harry S Truman ", "The better the 4-wheel drive, the further you'll be from help when ya get stuck!", "headmistressship", "aardvark", "😍😀🙌💃😍😍😍🙌"};
        int idx = 0;
        while (idx < strings.length) {
            String s = strings[idx];
            Object[] r = collapse(s);
            Object cs = r[0];
            Object olen = r[1];
            Object clen = r[2];
            System.out.println("original : length = " + String.valueOf(olen) + ", string = «««" + s + "»»»");
            System.out.println("collapsed: length = " + String.valueOf(clen) + ", string = «««" + (String)(cs) + "»»»\n");
            idx = idx + 1;
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
