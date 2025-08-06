public class Main {
    static java.util.Map<String,String> MORSE_CODE_DICT;
    static String[] MORSE_COMBINATIONS;
    static java.util.Map<String,String> REVERSE_DICT;
    static String plaintext;
    static String key;
    static String ciphertext;
    static String decrypted_1;

    static String encodeToMorse(String plaintext) {
        String morse = "";
        int i = 0;
        while (i < _runeLen(plaintext)) {
            String ch = plaintext.substring(i, i + 1).toUpperCase();
            String code = "";
            if (((Boolean)(MORSE_CODE_DICT.containsKey(ch)))) {
                code = ((String)(MORSE_CODE_DICT).get(ch));
            }
            if (i > 0) {
                morse = morse + "x";
            }
            morse = morse + code;
            i = i + 1;
        }
        return morse;
    }

    static String encryptFractionatedMorse(String plaintext, String key) {
        String morseCode = String.valueOf(encodeToMorse(plaintext));
        String combinedKey = key.toUpperCase() + "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String dedupKey = "";
        int i_1 = 0;
        while (i_1 < _runeLen(combinedKey)) {
            String ch_1 = combinedKey.substring(i_1, i_1 + 1);
            if (!(dedupKey.contains(ch_1))) {
                dedupKey = dedupKey + ch_1;
            }
            i_1 = i_1 + 1;
        }
        int paddingLength = 3 - (Math.floorMod(_runeLen(morseCode), 3));
        int p = 0;
        while (p < paddingLength) {
            morseCode = morseCode + "x";
            p = p + 1;
        }
        java.util.Map<String,String> dict = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>()));
        int j = 0;
        while (j < 26) {
            String combo = MORSE_COMBINATIONS[j];
            String letter = dedupKey.substring(j, j + 1);
dict.put(combo, letter);
            j = j + 1;
        }
dict.put("xxx", "");
        String encrypted = "";
        int k = 0;
        while (k < _runeLen(morseCode)) {
            String group = morseCode.substring(k, k + 3);
            encrypted = encrypted + ((String)(dict).get(group));
            k = k + 3;
        }
        return encrypted;
    }

    static String decryptFractionatedMorse(String ciphertext, String key) {
        String combinedKey_1 = key.toUpperCase() + "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String dedupKey_1 = "";
        int i_2 = 0;
        while (i_2 < _runeLen(combinedKey_1)) {
            String ch_2 = combinedKey_1.substring(i_2, i_2 + 1);
            if (!(dedupKey_1.contains(ch_2))) {
                dedupKey_1 = dedupKey_1 + ch_2;
            }
            i_2 = i_2 + 1;
        }
        java.util.Map<String,String> inv = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>()));
        int j_1 = 0;
        while (j_1 < 26) {
            String letter_1 = dedupKey_1.substring(j_1, j_1 + 1);
inv.put(letter_1, MORSE_COMBINATIONS[j_1]);
            j_1 = j_1 + 1;
        }
        String morse_1 = "";
        int k_1 = 0;
        while (k_1 < _runeLen(ciphertext)) {
            String ch_3 = ciphertext.substring(k_1, k_1 + 1);
            if (((Boolean)(inv.containsKey(ch_3)))) {
                morse_1 = morse_1 + ((String)(inv).get(ch_3));
            }
            k_1 = k_1 + 1;
        }
        String[] codes = ((String[])(new String[]{}));
        String current = "";
        int m = 0;
        while (m < _runeLen(morse_1)) {
            String ch_4 = morse_1.substring(m, m + 1);
            if ((ch_4.equals("x"))) {
                codes = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(codes), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                current = "";
            } else {
                current = current + ch_4;
            }
            m = m + 1;
        }
        codes = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(codes), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        String decrypted = "";
        int idx = 0;
        while (idx < codes.length) {
            String code_1 = codes[idx];
            decrypted = decrypted + ((String)(REVERSE_DICT).get(code_1));
            idx = idx + 1;
        }
        int start = 0;
        while (true) {
            if (start < _runeLen(decrypted)) {
                if ((decrypted.substring(start, start + 1).equals(" "))) {
                    start = start + 1;
                    continue;
                }
            }
            break;
        }
        int end = _runeLen(decrypted);
        while (true) {
            if (end > start) {
                if ((decrypted.substring(end - 1, end).equals(" "))) {
                    end = end - 1;
                    continue;
                }
            }
            break;
        }
        return decrypted.substring(start, end);
    }
    public static void main(String[] args) {
        MORSE_CODE_DICT = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("A", ".-"), java.util.Map.entry("B", "-..."), java.util.Map.entry("C", "-.-."), java.util.Map.entry("D", "-.."), java.util.Map.entry("E", "."), java.util.Map.entry("F", "..-."), java.util.Map.entry("G", "--."), java.util.Map.entry("H", "...."), java.util.Map.entry("I", ".."), java.util.Map.entry("J", ".---"), java.util.Map.entry("K", "-.-"), java.util.Map.entry("L", ".-.."), java.util.Map.entry("M", "--"), java.util.Map.entry("N", "-."), java.util.Map.entry("O", "---"), java.util.Map.entry("P", ".--."), java.util.Map.entry("Q", "--.-"), java.util.Map.entry("R", ".-."), java.util.Map.entry("S", "..."), java.util.Map.entry("T", "-"), java.util.Map.entry("U", "..-"), java.util.Map.entry("V", "...-"), java.util.Map.entry("W", ".--"), java.util.Map.entry("X", "-..-"), java.util.Map.entry("Y", "-.--"), java.util.Map.entry("Z", "--.."), java.util.Map.entry(" ", "")))));
        MORSE_COMBINATIONS = ((String[])(new String[]{"...", "..-", "..x", ".-.", ".--", ".-x", ".x.", ".x-", ".xx", "-..", "-.-", "-.x", "--.", "---", "--x", "-x.", "-x-", "-xx", "x..", "x.-", "x.x", "x-.", "x--", "x-x", "xx.", "xx-", "xxx"}));
        REVERSE_DICT = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry(".-", "A"), java.util.Map.entry("-...", "B"), java.util.Map.entry("-.-.", "C"), java.util.Map.entry("-..", "D"), java.util.Map.entry(".", "E"), java.util.Map.entry("..-.", "F"), java.util.Map.entry("--.", "G"), java.util.Map.entry("....", "H"), java.util.Map.entry("..", "I"), java.util.Map.entry(".---", "J"), java.util.Map.entry("-.-", "K"), java.util.Map.entry(".-..", "L"), java.util.Map.entry("--", "M"), java.util.Map.entry("-.", "N"), java.util.Map.entry("---", "O"), java.util.Map.entry(".--.", "P"), java.util.Map.entry("--.-", "Q"), java.util.Map.entry(".-.", "R"), java.util.Map.entry("...", "S"), java.util.Map.entry("-", "T"), java.util.Map.entry("..-", "U"), java.util.Map.entry("...-", "V"), java.util.Map.entry(".--", "W"), java.util.Map.entry("-..-", "X"), java.util.Map.entry("-.--", "Y"), java.util.Map.entry("--..", "Z"), java.util.Map.entry("", " ")))));
        plaintext = "defend the east";
        System.out.println("Plain Text:" + " " + plaintext);
        key = "ROUNDTABLE";
        ciphertext = String.valueOf(encryptFractionatedMorse(plaintext, key));
        System.out.println("Encrypted:" + " " + ciphertext);
        decrypted_1 = String.valueOf(decryptFractionatedMorse(ciphertext, key));
        System.out.println("Decrypted:" + " " + decrypted_1);
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
