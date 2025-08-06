public class Main {
    static int[][] donor_pref;
    static int[][] recipient_pref;

    static int index_of(int[] xs, int x) {
        int i = 0;
        while (i < xs.length) {
            if (xs[i] == x) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int[] remove_item(int[] xs, int x) {
        int[] res = ((int[])(new int[]{}));
        boolean removed = false;
        int i_1 = 0;
        while (i_1 < xs.length) {
            if (!removed && xs[i_1] == x) {
                removed = true;
            } else {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i_1])).toArray()));
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static int[] stable_matching(int[][] donor_pref, int[][] recipient_pref) {
        if (donor_pref.length != recipient_pref.length) {
            throw new RuntimeException(String.valueOf("unequal groups"));
        }
        int n = donor_pref.length;
        int[] unmatched = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < n) {
            unmatched = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(unmatched), java.util.stream.IntStream.of(i_2)).toArray()));
            i_2 = i_2 + 1;
        }
        int[] donor_record = ((int[])(new int[]{}));
        i_2 = 0;
        while (i_2 < n) {
            donor_record = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(donor_record), java.util.stream.IntStream.of(-1)).toArray()));
            i_2 = i_2 + 1;
        }
        int[] rec_record = ((int[])(new int[]{}));
        i_2 = 0;
        while (i_2 < n) {
            rec_record = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(rec_record), java.util.stream.IntStream.of(-1)).toArray()));
            i_2 = i_2 + 1;
        }
        int[] num_donations = ((int[])(new int[]{}));
        i_2 = 0;
        while (i_2 < n) {
            num_donations = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(num_donations), java.util.stream.IntStream.of(0)).toArray()));
            i_2 = i_2 + 1;
        }
        while (unmatched.length > 0) {
            int donor = unmatched[0];
            int[] donor_preference = ((int[])(donor_pref[donor]));
            int recipient = donor_preference[num_donations[donor]];
num_donations[donor] = num_donations[donor] + 1;
            int[] rec_preference = ((int[])(recipient_pref[recipient]));
            int prev_donor = rec_record[recipient];
            if (prev_donor != 0 - 1) {
                int prev_index = index_of(((int[])(rec_preference)), prev_donor);
                int new_index = index_of(((int[])(rec_preference)), donor);
                if (prev_index > new_index) {
rec_record[recipient] = donor;
donor_record[donor] = recipient;
                    unmatched = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(unmatched), java.util.stream.IntStream.of(prev_donor)).toArray()));
                    unmatched = ((int[])(remove_item(((int[])(unmatched)), donor)));
                }
            } else {
rec_record[recipient] = donor;
donor_record[donor] = recipient;
                unmatched = ((int[])(remove_item(((int[])(unmatched)), donor)));
            }
        }
        return donor_record;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            donor_pref = ((int[][])(new int[][]{new int[]{0, 1, 3, 2}, new int[]{0, 2, 3, 1}, new int[]{1, 0, 2, 3}, new int[]{0, 3, 1, 2}}));
            recipient_pref = ((int[][])(new int[][]{new int[]{3, 1, 2, 0}, new int[]{3, 1, 0, 2}, new int[]{0, 3, 1, 2}, new int[]{1, 0, 3, 2}}));
            System.out.println(_p(stable_matching(((int[][])(donor_pref)), ((int[][])(recipient_pref)))));
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

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
