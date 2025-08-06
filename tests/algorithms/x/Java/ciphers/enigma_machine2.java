public class Main {
    static String abc;
    static String low_abc;
    static String rotor1;
    static String rotor2;
    static String rotor3;
    static String rotor4;
    static String rotor5;
    static String rotor6;
    static String rotor7;
    static String rotor8;
    static String rotor9;
    static String[] reflector_pairs;

    static boolean list_contains(String[] xs, String x) {
        int i = 0;
        while (i < xs.length) {
            if ((xs[i].equals(x))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static int index_in_string(String s, String ch) {
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            if ((_substr(s, i_1, i_1 + 1).equals(ch))) {
                return i_1;
            }
            i_1 = i_1 + 1;
        }
        return -1;
    }

    static boolean contains_char(String s, String ch) {
        return index_in_string(s, ch) >= 0;
    }

    static String to_uppercase(String s) {
        String res = "";
        int i_2 = 0;
        while (i_2 < _runeLen(s)) {
            String ch = _substr(s, i_2, i_2 + 1);
            int idx = index_in_string(low_abc, ch);
            if (idx >= 0) {
                res = res + _substr(abc, idx, idx + 1);
            } else {
                res = res + ch;
            }
            i_2 = i_2 + 1;
        }
        return res;
    }

    static String plugboard_map(String[] pb, String ch) {
        int i_3 = 0;
        while (i_3 < pb.length) {
            String pair = pb[i_3];
            String a = _substr(pair, 0, 1);
            String b = _substr(pair, 1, 2);
            if ((ch.equals(a))) {
                return b;
            }
            if ((ch.equals(b))) {
                return a;
            }
            i_3 = i_3 + 1;
        }
        return ch;
    }

    static String reflector_map(String ch) {
        int i_4 = 0;
        while (i_4 < reflector_pairs.length) {
            String pair_1 = reflector_pairs[i_4];
            String a_1 = _substr(pair_1, 0, 1);
            String b_1 = _substr(pair_1, 1, 2);
            if ((ch.equals(a_1))) {
                return b_1;
            }
            if ((ch.equals(b_1))) {
                return a_1;
            }
            i_4 = i_4 + 1;
        }
        return ch;
    }

    static int count_unique(String[] xs) {
        String[] unique = ((String[])(new String[]{}));
        int i_5 = 0;
        while (i_5 < xs.length) {
            if (!(Boolean)list_contains(((String[])(unique)), xs[i_5])) {
                unique = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(unique), java.util.stream.Stream.of(xs[i_5])).toArray(String[]::new)));
            }
            i_5 = i_5 + 1;
        }
        return unique.length;
    }

    static String[] build_plugboard(String pbstring) {
        if (_runeLen(pbstring) == 0) {
            return new String[]{};
        }
        if (Math.floorMod(_runeLen(pbstring), 2) != 0) {
            throw new RuntimeException(String.valueOf("Odd number of symbols(" + _p(_runeLen(pbstring)) + ")"));
        }
        String pbstring_nospace = "";
        int i_6 = 0;
        while (i_6 < _runeLen(pbstring)) {
            String ch_1 = _substr(pbstring, i_6, i_6 + 1);
            if (!(ch_1.equals(" "))) {
                pbstring_nospace = pbstring_nospace + ch_1;
            }
            i_6 = i_6 + 1;
        }
        String[] seen = ((String[])(new String[]{}));
        i_6 = 0;
        while (i_6 < _runeLen(pbstring_nospace)) {
            String ch_2 = _substr(pbstring_nospace, i_6, i_6 + 1);
            if (!(Boolean)contains_char(abc, ch_2)) {
                throw new RuntimeException(String.valueOf("'" + ch_2 + "' not in list of symbols"));
            }
            if (((Boolean)(list_contains(((String[])(seen)), ch_2)))) {
                throw new RuntimeException(String.valueOf("Duplicate symbol(" + ch_2 + ")"));
            }
            seen = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(seen), java.util.stream.Stream.of(ch_2)).toArray(String[]::new)));
            i_6 = i_6 + 1;
        }
        String[] pb = ((String[])(new String[]{}));
        i_6 = 0;
        while (i_6 < _runeLen(pbstring_nospace) - 1) {
            String a_2 = _substr(pbstring_nospace, i_6, i_6 + 1);
            String b_2 = _substr(pbstring_nospace, i_6 + 1, i_6 + 2);
            pb = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(pb), java.util.stream.Stream.of(a_2 + b_2)).toArray(String[]::new)));
            i_6 = i_6 + 2;
        }
        return pb;
    }

    static void validator(int[] rotpos, String[] rotsel, String pb) {
        if (count_unique(((String[])(rotsel))) < 3) {
            throw new RuntimeException(String.valueOf("Please use 3 unique rotors (not " + _p(count_unique(((String[])(rotsel)))) + ")"));
        }
        if (rotpos.length != 3) {
            throw new RuntimeException(String.valueOf("Rotor position must have 3 values"));
        }
        int r1 = rotpos[0];
        int r2 = rotpos[1];
        int r3 = rotpos[2];
        if (!(0 < r1 && r1 <= _runeLen(abc))) {
            throw new RuntimeException(String.valueOf("First rotor position is not within range of 1..26 (" + _p(r1) + ")"));
        }
        if (!(0 < r2 && r2 <= _runeLen(abc))) {
            throw new RuntimeException(String.valueOf("Second rotor position is not within range of 1..26 (" + _p(r2) + ")"));
        }
        if (!(0 < r3 && r3 <= _runeLen(abc))) {
            throw new RuntimeException(String.valueOf("Third rotor position is not within range of 1..26 (" + _p(r3) + ")"));
        }
    }

    static String enigma(String text, int[] rotor_position, String[] rotor_selection, String plugb) {
        String up_text = String.valueOf(to_uppercase(text));
        String up_pb = String.valueOf(to_uppercase(plugb));
        validator(((int[])(rotor_position)), ((String[])(rotor_selection)), up_pb);
        String[] plugboard = ((String[])(build_plugboard(up_pb)));
        int rotorpos1 = rotor_position[0] - 1;
        int rotorpos2 = rotor_position[1] - 1;
        int rotorpos3 = rotor_position[2] - 1;
        String rotor_a = rotor_selection[0];
        String rotor_b = rotor_selection[1];
        String rotor_c = rotor_selection[2];
        String result = "";
        int i_7 = 0;
        while (i_7 < _runeLen(up_text)) {
            String symbol = _substr(up_text, i_7, i_7 + 1);
            if (((Boolean)(contains_char(abc, symbol)))) {
                symbol = String.valueOf(plugboard_map(((String[])(plugboard)), symbol));
                int index = index_in_string(abc, symbol) + rotorpos1;
                symbol = _substr(rotor_a, Math.floorMod(index, _runeLen(abc)), Math.floorMod(index, _runeLen(abc)) + 1);
                index = index_in_string(abc, symbol) + rotorpos2;
                symbol = _substr(rotor_b, Math.floorMod(index, _runeLen(abc)), Math.floorMod(index, _runeLen(abc)) + 1);
                index = index_in_string(abc, symbol) + rotorpos3;
                symbol = _substr(rotor_c, Math.floorMod(index, _runeLen(abc)), Math.floorMod(index, _runeLen(abc)) + 1);
                symbol = String.valueOf(reflector_map(symbol));
                index = index_in_string(rotor_c, symbol) - rotorpos3;
                if (index < 0) {
                    index = index + _runeLen(abc);
                }
                symbol = _substr(abc, index, index + 1);
                index = index_in_string(rotor_b, symbol) - rotorpos2;
                if (index < 0) {
                    index = index + _runeLen(abc);
                }
                symbol = _substr(abc, index, index + 1);
                index = index_in_string(rotor_a, symbol) - rotorpos1;
                if (index < 0) {
                    index = index + _runeLen(abc);
                }
                symbol = _substr(abc, index, index + 1);
                symbol = String.valueOf(plugboard_map(((String[])(plugboard)), symbol));
                rotorpos1 = rotorpos1 + 1;
                if (rotorpos1 >= _runeLen(abc)) {
                    rotorpos1 = 0;
                    rotorpos2 = rotorpos2 + 1;
                }
                if (rotorpos2 >= _runeLen(abc)) {
                    rotorpos2 = 0;
                    rotorpos3 = rotorpos3 + 1;
                }
                if (rotorpos3 >= _runeLen(abc)) {
                    rotorpos3 = 0;
                }
            }
            result = result + symbol;
            i_7 = i_7 + 1;
        }
        return result;
    }

    static void main() {
        String message = "This is my Python script that emulates the Enigma machine from WWII.";
        int[] rotor_pos = ((int[])(new int[]{1, 1, 1}));
        String pb_1 = "pictures";
        String[] rotor_sel = ((String[])(new String[]{rotor2, rotor4, rotor8}));
        String en = String.valueOf(enigma(message, ((int[])(rotor_pos)), ((String[])(rotor_sel)), pb_1));
        System.out.println("Encrypted message: " + en);
        System.out.println("Decrypted message: " + String.valueOf(enigma(en, ((int[])(rotor_pos)), ((String[])(rotor_sel)), pb_1)));
    }
    public static void main(String[] args) {
        abc = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        low_abc = "abcdefghijklmnopqrstuvwxyz";
        rotor1 = "EGZWVONAHDCLFQMSIPJBYUKXTR";
        rotor2 = "FOBHMDKEXQNRAULPGSJVTYICZW";
        rotor3 = "ZJXESIUQLHAVRMDOYGTNFWPBKC";
        rotor4 = "RMDJXFUWGISLHVTCQNKYPBEZOA";
        rotor5 = "SGLCPQWZHKXAREONTFBVIYJUDM";
        rotor6 = "HVSICLTYKQUBXDWAJZOMFGPREN";
        rotor7 = "RZWQHFMVDBKICJLNTUXAGYPSOE";
        rotor8 = "LFKIJODBEGAMQPXVUHYSTCZRWN";
        rotor9 = "KOAEGVDHXPQZMLFTYWJNBRCIUS";
        reflector_pairs = ((String[])(new String[]{"AN", "BO", "CP", "DQ", "ER", "FS", "GT", "HU", "IV", "JW", "KX", "LY", "MZ"}));
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
