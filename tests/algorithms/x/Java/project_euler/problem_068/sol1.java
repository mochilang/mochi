public class Main {

    static int[] range_desc(int start, int end) {
        int[] res = ((int[])(new int[]{}));
        int i = start;
        while (i >= end) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(i)).toArray()));
            i = i - 1;
        }
        return res;
    }

    static int[] range_asc(int start, int end) {
        int[] res_1 = ((int[])(new int[]{}));
        int i_1 = start;
        while (i_1 <= end) {
            res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(i_1)).toArray()));
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static int[] concat_lists(int[] a, int[] b) {
        int[] res_2 = ((int[])(a));
        int i_2 = 0;
        while (i_2 < b.length) {
            res_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_2), java.util.stream.IntStream.of(b[i_2])).toArray()));
            i_2 = i_2 + 1;
        }
        return res_2;
    }

    static int[] swap(int[] xs, int i, int j) {
        int[] res_3 = ((int[])(new int[]{}));
        int k = 0;
        while (k < xs.length) {
            if (k == i) {
                res_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_3), java.util.stream.IntStream.of(xs[j])).toArray()));
            } else             if (k == j) {
                res_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_3), java.util.stream.IntStream.of(xs[i])).toArray()));
            } else {
                res_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_3), java.util.stream.IntStream.of(xs[k])).toArray()));
            }
            k = k + 1;
        }
        return res_3;
    }

    static int[] generate_gon_ring(int gon_side, int[] perm) {
        int[] result = ((int[])(new int[]{}));
        result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(perm[0])).toArray()));
        result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(perm[1])).toArray()));
        result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(perm[2])).toArray()));
        int[] extended = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(perm), java.util.stream.IntStream.of(perm[1])).toArray()));
        int magic_number = gon_side < 5 ? 1 : 2;
        int i_3 = 1;
        while (i_3 < Math.floorDiv(extended.length, 3) + magic_number) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(extended[2 * i_3 + 1])).toArray()));
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(result[3 * i_3 - 1])).toArray()));
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(extended[2 * i_3 + 2])).toArray()));
            i_3 = i_3 + 1;
        }
        return result;
    }

    static int min_outer(int[] numbers) {
        int min_val = numbers[0];
        int i_4 = 3;
        while (i_4 < numbers.length) {
            if (numbers[i_4] < min_val) {
                min_val = numbers[i_4];
            }
            i_4 = i_4 + 3;
        }
        return min_val;
    }

    static boolean is_magic_gon(int[] numbers) {
        if (Math.floorMod(numbers.length, 3) != 0) {
            return false;
        }
        if (min_outer(((int[])(numbers))) != numbers[0]) {
            return false;
        }
        int total = numbers[0] + numbers[1] + numbers[2];
        int i_5 = 3;
        while (i_5 < numbers.length) {
            if (numbers[i_5] + numbers[i_5 + 1] + numbers[i_5 + 2] != total) {
                return false;
            }
            i_5 = i_5 + 3;
        }
        return true;
    }

    static String permute_search(int[] nums, int start, int gon_side, String current_max) {
        if (start == nums.length) {
            int[] ring = ((int[])(generate_gon_ring(gon_side, ((int[])(nums)))));
            if (((Boolean)(is_magic_gon(((int[])(ring)))))) {
                String s = "";
                int k_1 = 0;
                while (k_1 < ring.length) {
                    s = s + _p(_geti(ring, k_1));
                    k_1 = k_1 + 1;
                }
                if ((s.compareTo(current_max) > 0)) {
                    return s;
                }
            }
            return current_max;
        }
        String res_4 = current_max;
        int i_6 = start;
        while (i_6 < nums.length) {
            int[] swapped = ((int[])(swap(((int[])(nums)), start, i_6)));
            String candidate = String.valueOf(permute_search(((int[])(swapped)), start + 1, gon_side, res_4));
            if ((candidate.compareTo(res_4) > 0)) {
                res_4 = candidate;
            }
            i_6 = i_6 + 1;
        }
        return res_4;
    }

    static String solution(int gon_side) {
        if (gon_side < 3 || gon_side > 5) {
            return "";
        }
        int[] small = ((int[])(range_desc(gon_side + 1, 1)));
        int[] big = ((int[])(range_asc(gon_side + 2, gon_side * 2)));
        int[] numbers = ((int[])(concat_lists(((int[])(small)), ((int[])(big)))));
        String max_str = String.valueOf(permute_search(((int[])(numbers)), 0, gon_side, ""));
        return max_str;
    }
    public static void main(String[] args) {
        System.out.println(solution(5));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
