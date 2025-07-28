public class Main {
    static class Data1 {
        int pm;
        int g1;
        int s1;
        int g2;
        int s2;
        int d;
        Data1(int pm, int g1, int s1, int g2, int s2, int d) {
            this.pm = pm;
            this.g1 = g1;
            this.s1 = s1;
            this.g2 = g2;
            this.s2 = s2;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'pm': %s, 'g1': %s, 's1': %s, 'g2': %s, 's2': %s, 'd': %s}", String.valueOf(pm), String.valueOf(g1), String.valueOf(s1), String.valueOf(g2), String.valueOf(s2), String.valueOf(d));
        }
    }


    static String commatize(int n) {
        String s = String.valueOf(n);
        int i = _runeLen(s) - 3;
        while (i > 0) {
            s = s.substring(0, i) + "," + s.substring(i, _runeLen(s));
            i = i - 3;
        }
        return s;
    }

    static void main() {
        Data1[] data = new Data1[]{new Data1(10, 4, 7, 6, 23, 16), new Data1(100, 14, 113, 16, 1831, 1718), new Data1(1000, 14, 113, 16, 1831, 1718), new Data1(10000, 36, 9551, 38, 30593, 21042), new Data1(100000, 70, 173359, 72, 31397, 141962), new Data1(1000000, 100, 396733, 102, 1444309, 1047576), new Data1(10000000, 148, 2010733, 150, 13626257, 11615524), new Data1(100000000, 198, 46006769, 200, 378043979, 332037210), new Data1(1000000000, 276, 649580171, 278, (int)4260928601L, (int)3611348430L), new Data1((int)10000000000L, 332, (int)5893180121L, 334, (int)30827138509L, (int)24933958388L), new Data1((int)100000000000L, 386, (int)35238645587L, 388, (int)156798792223L, (int)121560146636L)};
        for (Data1 entry : data) {
            String pm = String.valueOf(commatize(entry.pm));
            String line1 = "Earliest difference > " + pm + " between adjacent prime gap starting primes:";
            System.out.println(line1);
            String line2 = "Gap " + String.valueOf(entry.g1) + " starts at " + String.valueOf(commatize(entry.s1)) + ", gap " + String.valueOf(entry.g2) + " starts at " + String.valueOf(commatize(entry.s2)) + ", difference is " + String.valueOf(commatize(entry.d)) + ".";
            System.out.println(line2);
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
