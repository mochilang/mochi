public class Main {
    static String ASCII;
    static String message;
    static int token;
    static String encoded;

    static String[] build_alphabet() {
        String[] result = ((String[])(new String[]{}));
        int i = 0;
        while (i < _runeLen(ASCII)) {
            result = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(ASCII.substring(i, i+1))).toArray(String[]::new)));
            i = i + 1;
        }
        return result;
    }

    static int[] range_list(int n) {
        int[] lst = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < n) {
            lst = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(lst), java.util.stream.IntStream.of(i_1)).toArray()));
            i_1 = i_1 + 1;
        }
        return lst;
    }

    static int[] reversed_range_list(int n) {
        int[] lst_1 = ((int[])(new int[]{}));
        int i_2 = n - 1;
        while (i_2 >= 0) {
            lst_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(lst_1), java.util.stream.IntStream.of(i_2)).toArray()));
            i_2 = i_2 - 1;
        }
        return lst_1;
    }

    static int index_of_char(String[] lst, String ch) {
        int i_3 = 0;
        while (i_3 < lst.length) {
            if ((lst[i_3].equals(ch))) {
                return i_3;
            }
            i_3 = i_3 + 1;
        }
        return -1;
    }

    static int index_of_int(int[] lst, int value) {
        int i_4 = 0;
        while (i_4 < lst.length) {
            if (lst[i_4] == value) {
                return i_4;
            }
            i_4 = i_4 + 1;
        }
        return -1;
    }

    static String enigma_encrypt(String message, int token) {
        String[] alphabets = ((String[])(build_alphabet()));
        int n = alphabets.length;
        int[][] gear_one = new int[1][];
        gear_one[0] = ((int[])(range_list(n)));
        int[][] gear_two = new int[1][];
        gear_two[0] = ((int[])(range_list(n)));
        int[][] gear_three = new int[1][];
        gear_three[0] = ((int[])(range_list(n)));
        int[] reflector = ((int[])(reversed_range_list(n)));
        int[] gear_one_pos = new int[1];
        gear_one_pos[0] = 0;
        int[] gear_two_pos = new int[1];
        gear_two_pos[0] = 0;
        int[] gear_three_pos = new int[1];
        gear_three_pos[0] = 0;
        Runnable[] rotator = new Runnable[1];
        rotator[0] = () -> {
        int i_5 = gear_one[0][0];
        gear_one[0] = ((int[])(java.util.Arrays.copyOfRange(gear_one[0], 1, gear_one[0].length)));
        gear_one[0] = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(gear_one[0]), java.util.stream.IntStream.of(i_5)).toArray()));
        gear_one_pos[0] = gear_one_pos[0] + 1;
        if (Math.floorMod(gear_one_pos[0], n) == 0) {
            i_5 = gear_two[0][0];
            gear_two[0] = ((int[])(java.util.Arrays.copyOfRange(gear_two[0], 1, gear_two[0].length)));
            gear_two[0] = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(gear_two[0]), java.util.stream.IntStream.of(i_5)).toArray()));
            gear_two_pos[0] = gear_two_pos[0] + 1;
            if (Math.floorMod(gear_two_pos[0], n) == 0) {
                i_5 = gear_three[0][0];
                gear_three[0] = ((int[])(java.util.Arrays.copyOfRange(gear_three[0], 1, gear_three[0].length)));
                gear_three[0] = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(gear_three[0]), java.util.stream.IntStream.of(i_5)).toArray()));
                gear_three_pos[0] = gear_three_pos[0] + 1;
            }
        }
};
        java.util.function.Function<String,String>[] engine = new java.util.function.Function[1];
        engine[0] = (ch) -> {
        int target = index_of_char(((String[])(alphabets)), ch);
        target = gear_one[0][target];
        target = gear_two[0][target];
        target = gear_three[0][target];
        target = reflector[target];
        target = index_of_int(((int[])(gear_three[0])), target);
        target = index_of_int(((int[])(gear_two[0])), target);
        target = index_of_int(((int[])(gear_one[0])), target);
        rotator[0].run();
        return alphabets[target];
};
        int t = 0;
        while (t < token) {
            rotator[0].run();
            t = t + 1;
        }
        String result_1 = "";
        int idx = 0;
        while (idx < _runeLen(message)) {
            result_1 = result_1 + String.valueOf(engine[0].apply(message.substring(idx, idx+1)));
            idx = idx + 1;
        }
        return result_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            ASCII = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}";
            message = "HELLO WORLD";
            token = 123;
            encoded = String.valueOf(enigma_encrypt(message, token));
            System.out.println(encoded);
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
