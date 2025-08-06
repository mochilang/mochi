public class Main {
    static String LETTERS;

    static int find_char(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((s.substring(i, i+1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String encrypt_message(String key, String message) {
        String chars_a = key;
        String chars_b = LETTERS;
        String translated = "";
        int i_1 = 0;
        while (i_1 < _runeLen(message)) {
            String symbol = message.substring(i_1, i_1+1);
            String upper_sym = symbol.toUpperCase();
            int sym_index = find_char(chars_a, upper_sym);
            if (sym_index >= 0) {
                String sub_char = chars_b.substring(sym_index, sym_index+1);
                if ((symbol.equals(upper_sym))) {
                    translated = translated + sub_char.toUpperCase();
                } else {
                    translated = translated + sub_char.toLowerCase();
                }
            } else {
                translated = translated + symbol;
            }
            i_1 = i_1 + 1;
        }
        return translated;
    }

    static String decrypt_message(String key, String message) {
        String chars_a_1 = LETTERS;
        String chars_b_1 = key;
        String translated_1 = "";
        int i_2 = 0;
        while (i_2 < _runeLen(message)) {
            String symbol_1 = message.substring(i_2, i_2+1);
            String upper_sym_1 = symbol_1.toUpperCase();
            int sym_index_1 = find_char(chars_a_1, upper_sym_1);
            if (sym_index_1 >= 0) {
                String sub_char_1 = chars_b_1.substring(sym_index_1, sym_index_1+1);
                if ((symbol_1.equals(upper_sym_1))) {
                    translated_1 = translated_1 + sub_char_1.toUpperCase();
                } else {
                    translated_1 = translated_1 + sub_char_1.toLowerCase();
                }
            } else {
                translated_1 = translated_1 + symbol_1;
            }
            i_2 = i_2 + 1;
        }
        return translated_1;
    }

    static void main() {
        String message = "Hello World";
        String key = "QWERTYUIOPASDFGHJKLZXCVBNM";
        String mode = "decrypt";
        String translated_2 = "";
        if ((mode.equals("encrypt"))) {
            translated_2 = String.valueOf(encrypt_message(key, message));
        } else         if ((mode.equals("decrypt"))) {
            translated_2 = String.valueOf(decrypt_message(key, message));
        }
        System.out.println("Using the key " + key + ", the " + mode + "ed message is: " + translated_2);
    }
    public static void main(String[] args) {
        LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
