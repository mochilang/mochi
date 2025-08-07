public class Main {

    static String get_digits(int num) {
        int cube = num * num * num;
        String s = _p(cube);
        int[] counts = ((int[])(new int[]{}));
        int j = 0;
        while (j < 10) {
            counts = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(counts), java.util.stream.IntStream.of(0)).toArray()));
            j = j + 1;
        }
        int i = 0;
        while (i < _runeLen(s)) {
            int d = Integer.parseInt(s.substring(i, i+1));
counts[d] = counts[d] + 1;
            i = i + 1;
        }
        String result = "";
        int d_1 = 0;
        while (d_1 < 10) {
            int c = counts[d_1];
            while (c > 0) {
                result = result + _p(d_1);
                c = c - 1;
            }
            d_1 = d_1 + 1;
        }
        return result;
    }

    static int solution(int max_base) {
        java.util.Map<String,int[]> freqs = ((java.util.Map<String,int[]>)(new java.util.LinkedHashMap<String, int[]>()));
        int num = 0;
        while (true) {
            String digits = String.valueOf(get_digits(num));
            int[] arr = ((int[])(new int[]{}));
            if (((Boolean)(freqs.containsKey(digits)))) {
                arr = (int[])(((int[])(freqs).get(digits)));
            }
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(num)).toArray()));
freqs.put(digits, ((int[])(arr)));
            if (arr.length == max_base) {
                int base = arr[0];
                return base * base * base;
            }
            num = num + 1;
        }
    }
    public static void main(String[] args) {
        System.out.println("solution() = " + _p(solution(5)));
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
