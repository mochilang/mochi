public class Main {
    static int[] s4 = new int[]{4, 3, 3, 3, 1, 2, 0, 2, 3};
    static int[] s1 = new int[]{1, 2, 0, 2, 1, 1, 0, 1, 3};
    static int[] s2 = new int[]{2, 1, 3, 1, 0, 1, 0, 1, 0};
    static int[] s3_a = plus(s1, s2);
    static int[] s3_b = plus(s2, s1);
    static int[] s3 = new int[]{3, 3, 3, 3, 3, 3, 3, 3, 3};
    static int[] s3_id = new int[]{2, 1, 2, 1, 0, 1, 2, 1, 2};
    static int[] s4b = plus(s3, s3_id);
    static int[] s5 = plus(s3_id, s3_id);

    static int[][] neighborsList() {
        return new int[][]{new int[]{1, 3}, new int[]{0, 2, 4}, new int[]{1, 5}, new int[]{0, 4, 6}, new int[]{1, 3, 5, 7}, new int[]{2, 4, 8}, new int[]{3, 7}, new int[]{4, 6, 8}, new int[]{5, 7}};
    }

    static int[] plus(int[] a, int[] b) {
        int[] res = new int[]{};
        int i = 0;
        while (i < a.length) {
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(((Number)(a[i])).intValue() + ((Number)(b[i])).intValue())).toArray();
            i = i + 1;
        }
        return res;
    }

    static boolean isStable(int[] p) {
        for (var v : p) {
            if (v > 3) {
                return false;
            }
        }
        return true;
    }

    static int topple(int[] p) {
        int[][] neighbors = neighborsList();
        int i = 0;
        while (i < p.length) {
            if (((Number)(p[i])).intValue() > 3) {
p[i] = ((Number)(p[i])).intValue() - 4;
                int[] nbs = neighbors[i];
                for (var j : nbs) {
p[j] = ((Number)(p[j])).intValue() + 1;
                }
                return 0;
            }
            i = i + 1;
        }
        return 0;
    }

    static String pileString(int[] p) {
        String s = "";
        int r = 0;
        while (r < 3) {
            int c = 0;
            while (c < 3) {
                s = s + String.valueOf(p[3 * r + c]) + " ";
                c = c + 1;
            }
            s = s + "\n";
            r = r + 1;
        }
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("Avalanche of topplings:\n");
            System.out.println(pileString(s4));
            while (!(Boolean)isStable(s4)) {
                topple(s4);
                System.out.println(pileString(s4));
            }
            System.out.println("Commutative additions:\n");
            while (!(Boolean)isStable(s3_a)) {
                topple(s3_a);
            }
            while (!(Boolean)isStable(s3_b)) {
                topple(s3_b);
            }
            System.out.println(pileString(s1) + "\nplus\n\n" + pileString(s2) + "\nequals\n\n" + pileString(s3_a));
            System.out.println("and\n\n" + pileString(s2) + "\nplus\n\n" + pileString(s1) + "\nalso equals\n\n" + pileString(s3_b));
            System.out.println("Addition of identity sandpile:\n");
            while (!(Boolean)isStable(s4b)) {
                topple(s4b);
            }
            System.out.println(pileString(s3) + "\nplus\n\n" + pileString(s3_id) + "\nequals\n\n" + pileString(s4b));
            System.out.println("Addition of identities:\n");
            while (!(Boolean)isStable(s5)) {
                topple(s5);
            }
            System.out.println(pileString(s3_id) + "\nplus\n\n" + pileString(s3_id) + "\nequals\n\n" + pileString(s5));
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
