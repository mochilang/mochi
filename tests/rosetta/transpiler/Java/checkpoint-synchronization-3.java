public class Main {
    static String[] partList = new String[]{"A", "B", "C", "D"};
    static int nAssemblies = 3;

    static String lower(String ch) {
        String up = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String low = "abcdefghijklmnopqrstuvwxyz";
        int i = 0;
        while (i < up.length()) {
            if ((ch.equals(up.substring(i, i + 1)))) {
                return low.substring(i, i + 1);
            }
            i = i + 1;
        }
        return ch;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            for (String p : partList) {
                System.out.println(p + " worker running");
            }
            for (int cycle = 1; cycle < (nAssemblies + 1); cycle++) {
                System.out.println("begin assembly cycle " + String.valueOf(cycle));
                String a = "";
                for (String p : partList) {
                    System.out.println(p + " worker begins part");
                    System.out.println(p + " worker completed " + p.toLowerCase());
                    a = a + p.toLowerCase();
                }
                System.out.println(a + " assembled.  cycle " + String.valueOf(cycle) + " complete");
            }
            for (String p : partList) {
                System.out.println(p + " worker stopped");
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
}
