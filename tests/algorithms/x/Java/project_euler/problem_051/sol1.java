public class Main {

    static int parse_int(String s) {
        int value = 0;
        int i = 0;
        while (i < _runeLen(s)) {
            value = value * 10 + (s.substring(i, i+1));
            i = i + 1;
        }
        return value;
    }

    static int[][] digit_replacements(int number) {
        String num_str = _p(number);
        int[] counts = ((int[])(new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
        int i_1 = 0;
        while (i_1 < _runeLen(num_str)) {
            int d = (num_str.substring(i_1, i_1+1));
counts[d] = counts[d] + 1;
            i_1 = i_1 + 1;
        }
        int[][] result = ((int[][])(new int[][]{}));
        String digits = "0123456789";
        int digit = 0;
        while (digit < 10) {
            if (counts[digit] > 1) {
                int[] family = ((int[])(new int[]{}));
                int repl = 0;
                while (repl < 10) {
                    String new_str = "";
                    int j = 0;
                    while (j < _runeLen(num_str)) {
                        String c = num_str.substring(j, j+1);
                        if ((c.equals(digits.substring(digit, digit+1)))) {
                            new_str = new_str + digits.substring(repl, repl+1);
                        } else {
                            new_str = new_str + c;
                        }
                        j = j + 1;
                    }
                    family = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(family), java.util.stream.IntStream.of(parse_int(new_str))).toArray()));
                    repl = repl + 1;
                }
                result = ((int[][])(appendObj(result, family)));
            }
            digit = digit + 1;
        }
        return result;
    }

    static boolean is_prime(int num) {
        if (num < 2) {
            return false;
        }
        if (Math.floorMod(num, 2) == 0) {
            return num == 2;
        }
        int i_2 = 3;
        while (i_2 * i_2 <= num) {
            if (Math.floorMod(num, i_2) == 0) {
                return false;
            }
            i_2 = i_2 + 2;
        }
        return true;
    }

    static int solution(int family_length) {
        int candidate = 121313;
        if (!(Boolean)is_prime(candidate)) {
            return -1;
        }
        int[][] reps = ((int[][])(digit_replacements(candidate)));
        int r = 0;
        while (r < reps.length) {
            int[] family_1 = ((int[])(reps[r]));
            int count = 0;
            int min_prime = 0;
            boolean first = true;
            int i_3 = 0;
            while (i_3 < family_1.length) {
                int num = family_1[i_3];
                if (((Boolean)(is_prime(num)))) {
                    if (first) {
                        min_prime = num;
                        first = false;
                    } else                     if (num < min_prime) {
                        min_prime = num;
                    }
                    count = count + 1;
                }
                i_3 = i_3 + 1;
            }
            if (count == family_length) {
                return min_prime;
            }
            r = r + 1;
        }
        return -1;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution(8)));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
