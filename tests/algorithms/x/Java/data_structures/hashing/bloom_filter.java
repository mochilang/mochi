public class Main {
    static String ascii;
    static class Bloom {
        int size;
        int[] bits;
        Bloom(int size, int[] bits) {
            this.size = size;
            this.bits = bits;
        }
        Bloom() {}
        @Override public String toString() {
            return String.format("{'size': %s, 'bits': %s}", String.valueOf(size), String.valueOf(bits));
        }
    }


    static int ord(String ch) {
        int i = 0;
        while (i < _runeLen(ascii)) {
            if ((ascii.substring(i, i + 1).equals(ch))) {
                return 32 + i;
            }
            i = i + 1;
        }
        return 0;
    }

    static Bloom new_bloom(int size) {
        int[] bits = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < size) {
            bits = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(bits), java.util.stream.IntStream.of(0)).toArray()));
            i_1 = i_1 + 1;
        }
        return new Bloom(size, bits);
    }

    static int hash1(String value, int size) {
        int h = 0;
        int i_2 = 0;
        while (i_2 < _runeLen(value)) {
            h = Math.floorMod((h * 31 + ord(value.substring(i_2, i_2 + 1))), size);
            i_2 = i_2 + 1;
        }
        return h;
    }

    static int hash2(String value, int size) {
        int h_1 = 0;
        int i_3 = 0;
        while (i_3 < _runeLen(value)) {
            h_1 = Math.floorMod((h_1 * 131 + ord(value.substring(i_3, i_3 + 1))), size);
            i_3 = i_3 + 1;
        }
        return h_1;
    }

    static int[] hash_positions(String value, int size) {
        int h1 = hash1(value, size);
        int h2 = hash2(value, size);
        int[] res = ((int[])(new int[]{}));
        res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(h1)).toArray()));
        res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(h2)).toArray()));
        return res;
    }

    static Bloom bloom_add(Bloom b, String value) {
        int[] pos = ((int[])(hash_positions(value, b.size)));
        int[] bits_1 = ((int[])(b.bits));
        int i_4 = 0;
        while (i_4 < pos.length) {
            int idx = b.size - 1 - pos[i_4];
bits_1[idx] = 1;
            i_4 = i_4 + 1;
        }
        return new Bloom(b.size, bits_1);
    }

    static boolean bloom_exists(Bloom b, String value) {
        int[] pos_1 = ((int[])(hash_positions(value, b.size)));
        int i_5 = 0;
        while (i_5 < pos_1.length) {
            int idx_1 = b.size - 1 - pos_1[i_5];
            if (b.bits[idx_1] != 1) {
                return false;
            }
            i_5 = i_5 + 1;
        }
        return true;
    }

    static String bitstring(Bloom b) {
        String res_1 = "";
        int i_6 = 0;
        while (i_6 < b.size) {
            res_1 = res_1 + _p(_geti(b.bits, i_6));
            i_6 = i_6 + 1;
        }
        return res_1;
    }

    static String format_hash(Bloom b, String value) {
        int[] pos_2 = ((int[])(hash_positions(value, b.size)));
        int[] bits_2 = ((int[])(new int[]{}));
        int i_7 = 0;
        while (i_7 < b.size) {
            bits_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(bits_2), java.util.stream.IntStream.of(0)).toArray()));
            i_7 = i_7 + 1;
        }
        i_7 = 0;
        while (i_7 < pos_2.length) {
            int idx_2 = b.size - 1 - pos_2[i_7];
bits_2[idx_2] = 1;
            i_7 = i_7 + 1;
        }
        String res_2 = "";
        i_7 = 0;
        while (i_7 < b.size) {
            res_2 = res_2 + _p(_geti(bits_2, i_7));
            i_7 = i_7 + 1;
        }
        return res_2;
    }

    static double estimated_error_rate(Bloom b) {
        int ones = 0;
        int i_8 = 0;
        while (i_8 < b.size) {
            if (b.bits[i_8] == 1) {
                ones = ones + 1;
            }
            i_8 = i_8 + 1;
        }
        double frac = (((Number)(ones)).doubleValue()) / (((Number)(b.size)).doubleValue());
        return frac * frac;
    }

    static boolean any_in(Bloom b, String[] items) {
        int i_9 = 0;
        while (i_9 < items.length) {
            if (((Boolean)(bloom_exists(b, items[i_9])))) {
                return true;
            }
            i_9 = i_9 + 1;
        }
        return false;
    }

    static void main() {
        Bloom bloom = new_bloom(8);
        System.out.println(bitstring(bloom));
        System.out.println(bloom_exists(bloom, "Titanic"));
        bloom = bloom_add(bloom, "Titanic");
        System.out.println(bitstring(bloom));
        System.out.println(bloom_exists(bloom, "Titanic"));
        bloom = bloom_add(bloom, "Avatar");
        System.out.println(bloom_exists(bloom, "Avatar"));
        System.out.println(format_hash(bloom, "Avatar"));
        System.out.println(bitstring(bloom));
        String[] not_present = ((String[])(new String[]{"The Godfather", "Interstellar", "Parasite", "Pulp Fiction"}));
        int i_10 = 0;
        while (i_10 < not_present.length) {
            String film = not_present[i_10];
            System.out.println(film + ":" + String.valueOf(format_hash(bloom, film)));
            i_10 = i_10 + 1;
        }
        System.out.println(any_in(bloom, ((String[])(not_present))));
        System.out.println(bloom_exists(bloom, "Ratatouille"));
        System.out.println(format_hash(bloom, "Ratatouille"));
        System.out.println(_p(estimated_error_rate(bloom)));
        bloom = bloom_add(bloom, "The Godfather");
        System.out.println(_p(estimated_error_rate(bloom)));
        System.out.println(bitstring(bloom));
    }
    public static void main(String[] args) {
        ascii = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
        main();
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
