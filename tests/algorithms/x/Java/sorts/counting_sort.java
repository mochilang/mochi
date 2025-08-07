public class Main {
    static String ascii_chars;
    static int[] example1;
    static int[] example2;
    static int[] example3;

    static int max_val(int[] arr) {
        int m = arr[0];
        int i = 1;
        while (i < arr.length) {
            if (arr[i] > m) {
                m = arr[i];
            }
            i = i + 1;
        }
        return m;
    }

    static int min_val(int[] arr) {
        int m_1 = arr[0];
        int i_1 = 1;
        while (i_1 < arr.length) {
            if (arr[i_1] < m_1) {
                m_1 = arr[i_1];
            }
            i_1 = i_1 + 1;
        }
        return m_1;
    }

    static int[] counting_sort(int[] collection) {
        if (collection.length == 0) {
            return new int[]{};
        }
        int coll_len = collection.length;
        int coll_max = max_val(((int[])(collection)));
        int coll_min = min_val(((int[])(collection)));
        int counting_arr_length = coll_max + 1 - coll_min;
        int[] counting_arr = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < counting_arr_length) {
            counting_arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(counting_arr), java.util.stream.IntStream.of(0)).toArray()));
            i_2 = i_2 + 1;
        }
        i_2 = 0;
        while (i_2 < coll_len) {
            int number = collection[i_2];
counting_arr[number - coll_min] = counting_arr[number - coll_min] + 1;
            i_2 = i_2 + 1;
        }
        i_2 = 1;
        while (i_2 < counting_arr_length) {
counting_arr[i_2] = counting_arr[i_2] + counting_arr[i_2 - 1];
            i_2 = i_2 + 1;
        }
        int[] ordered = ((int[])(new int[]{}));
        i_2 = 0;
        while (i_2 < coll_len) {
            ordered = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ordered), java.util.stream.IntStream.of(0)).toArray()));
            i_2 = i_2 + 1;
        }
        int idx = coll_len - 1;
        while (idx >= 0) {
            int number_1 = collection[idx];
            int pos = counting_arr[number_1 - coll_min] - 1;
ordered[pos] = number_1;
counting_arr[number_1 - coll_min] = counting_arr[number_1 - coll_min] - 1;
            idx = idx - 1;
        }
        return ordered;
    }

    static String chr(int code) {
        if (code == 10) {
            return "\n";
        }
        if (code == 13) {
            return "\r";
        }
        if (code == 9) {
            return "\t";
        }
        if (code >= 32 && code < 127) {
            return ascii_chars.substring(code - 32, code - 31);
        }
        return "";
    }

    static int ord(String ch) {
        if ((ch.equals("\n"))) {
            return 10;
        }
        if ((ch.equals("\r"))) {
            return 13;
        }
        if ((ch.equals("\t"))) {
            return 9;
        }
        int i_3 = 0;
        while (i_3 < _runeLen(ascii_chars)) {
            if ((ascii_chars.substring(i_3, i_3 + 1).equals(ch))) {
                return 32 + i_3;
            }
            i_3 = i_3 + 1;
        }
        return 0;
    }

    static String counting_sort_string(String s) {
        int[] codes = ((int[])(new int[]{}));
        int i_4 = 0;
        while (i_4 < _runeLen(s)) {
            codes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(codes), java.util.stream.IntStream.of(ord(s.substring(i_4, i_4 + 1)))).toArray()));
            i_4 = i_4 + 1;
        }
        int[] sorted_codes = ((int[])(counting_sort(((int[])(codes)))));
        String res = "";
        i_4 = 0;
        while (i_4 < sorted_codes.length) {
            res = res + String.valueOf(chr(sorted_codes[i_4]));
            i_4 = i_4 + 1;
        }
        return res;
    }
    public static void main(String[] args) {
        ascii_chars = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
        example1 = ((int[])(counting_sort(((int[])(new int[]{0, 5, 3, 2, 2})))));
        System.out.println(_p(example1));
        example2 = ((int[])(counting_sort(((int[])(new int[]{})))));
        System.out.println(_p(example2));
        example3 = ((int[])(counting_sort(((int[])(new int[]{-2, -5, -45})))));
        System.out.println(_p(example3));
        System.out.println(counting_sort_string("thisisthestring"));
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
