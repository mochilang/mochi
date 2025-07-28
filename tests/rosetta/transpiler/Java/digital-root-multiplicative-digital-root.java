public class Main {
    static class MDRResult {
        int mp;
        int mdr;
        MDRResult(int mp, int mdr) {
            this.mp = mp;
            this.mdr = mdr;
        }
        @Override public String toString() {
            return String.format("{'mp': %s, 'mdr': %s}", String.valueOf(mp), String.valueOf(mdr));
        }
    }


    static String pad(String s, int width) {
        String out = s;
        while (_runeLen(out) < width) {
            out = " " + out;
        }
        return out;
    }

    static java.math.BigInteger mult(java.math.BigInteger n, int base) {
        java.math.BigInteger m = java.math.BigInteger.valueOf(1);
        java.math.BigInteger x = n;
        java.math.BigInteger b = new java.math.BigInteger(String.valueOf(base));
        while (x.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            m = m.multiply((x.remainder(b)));
            x = x.divide(b);
        }
        return m;
    }

    static MDRResult multDigitalRoot(java.math.BigInteger n, int base) {
        java.math.BigInteger m = n;
        int mp = 0;
        java.math.BigInteger b = new java.math.BigInteger(String.valueOf(base));
        while (m.compareTo(b) >= 0) {
            m = mult(m, base);
            mp = mp + 1;
        }
        return new MDRResult(mp, (((Number)(m)).intValue()));
    }

    static void main() {
        int base = 10;
        int size = 5;
        System.out.println(String.valueOf(pad("Number", 20)) + " " + String.valueOf(pad("MDR", 3)) + " " + String.valueOf(pad("MP", 3)));
        java.math.BigInteger[] nums = new java.math.BigInteger[]{java.math.BigInteger.valueOf(123321), java.math.BigInteger.valueOf(7739), java.math.BigInteger.valueOf(893), java.math.BigInteger.valueOf(899998), java.math.BigInteger.valueOf((int)3778888999L), java.math.BigInteger.valueOf((int)277777788888899L)};
        int i = 0;
        while (i < nums.length) {
            java.math.BigInteger n = nums[i];
            MDRResult r = multDigitalRoot(n, base);
            System.out.println(String.valueOf(pad(String.valueOf(n), 20)) + " " + String.valueOf(pad(String.valueOf(r.mdr), 3)) + " " + String.valueOf(pad(String.valueOf(r.mp), 3)));
            i = i + 1;
        }
        System.out.println("");
        int[][] list = new int[][]{};
        int idx = 0;
        while (idx < base) {
            list = appendObj(list, new int[]{});
            idx = idx + 1;
        }
        int cnt = size * base;
        java.math.BigInteger n = java.math.BigInteger.valueOf(0);
        java.math.BigInteger b = new java.math.BigInteger(String.valueOf(base));
        while (cnt > 0) {
            MDRResult r = multDigitalRoot(n, base);
            int mdr = r.mdr;
            if (list[mdr].length < size) {
list[mdr] = java.util.stream.IntStream.concat(java.util.Arrays.stream(list[mdr]), java.util.stream.IntStream.of(((Number)(n)).intValue())).toArray();
                cnt = cnt - 1;
            }
            n = n.add(java.math.BigInteger.valueOf(1));
        }
        System.out.println("MDR: First");
        int j = 0;
        while (j < base) {
            System.out.println(String.valueOf(pad(String.valueOf(j), 3)) + ": " + String.valueOf(list[j]));
            j = j + 1;
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
