public class Main {
    static class Result {
        int shift;
        double chi;
        String decoded;
        Result(int shift, double chi, String decoded) {
            this.shift = shift;
            this.chi = chi;
            this.decoded = decoded;
        }
        Result() {}
        @Override public String toString() {
            return String.format("{'shift': %s, 'chi': %s, 'decoded': '%s'}", String.valueOf(shift), String.valueOf(chi), String.valueOf(decoded));
        }
    }

    static Result r1;
    static Result r2;
    static Result r3;

    static String[] default_alphabet() {
        return new String[]{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"};
    }

    static java.util.Map<String,Double> default_frequencies() {
        return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("a", 0.08497), java.util.Map.entry("b", 0.01492), java.util.Map.entry("c", 0.02202), java.util.Map.entry("d", 0.04253), java.util.Map.entry("e", 0.11162), java.util.Map.entry("f", 0.02228), java.util.Map.entry("g", 0.02015), java.util.Map.entry("h", 0.06094), java.util.Map.entry("i", 0.07546), java.util.Map.entry("j", 0.00153), java.util.Map.entry("k", 0.01292), java.util.Map.entry("l", 0.04025), java.util.Map.entry("m", 0.02406), java.util.Map.entry("n", 0.06749), java.util.Map.entry("o", 0.07507), java.util.Map.entry("p", 0.01929), java.util.Map.entry("q", 0.00095), java.util.Map.entry("r", 0.07587), java.util.Map.entry("s", 0.06327), java.util.Map.entry("t", 0.09356), java.util.Map.entry("u", 0.02758), java.util.Map.entry("v", 0.00978), java.util.Map.entry("w", 0.0256), java.util.Map.entry("x", 0.0015), java.util.Map.entry("y", 0.01994), java.util.Map.entry("z", 0.00077)));
    }

    static int index_of(String[] xs, String ch) {
        int i = 0;
        while (i < xs.length) {
            if ((xs[i].equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int count_char(String s, String ch) {
        int count = 0;
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            if ((_substr(s, i_1, i_1 + 1).equals(ch))) {
                count = count + 1;
            }
            i_1 = i_1 + 1;
        }
        return count;
    }

    static Result decrypt_caesar_with_chi_squared(String ciphertext, String[] cipher_alphabet, java.util.Map<String,Double> frequencies_dict, boolean case_sensitive) {
        String[] alphabet_letters = ((String[])(cipher_alphabet));
        if (alphabet_letters.length == 0) {
            alphabet_letters = ((String[])(default_alphabet()));
        }
        java.util.Map<String,Double> frequencies = frequencies_dict;
        if (frequencies.size() == 0) {
            frequencies = default_frequencies();
        }
        if (!(Boolean)case_sensitive) {
            ciphertext = ciphertext.toLowerCase();
        }
        int best_shift = 0;
        double best_chi = 0.0;
        String best_text = "";
        int shift = 0;
        while (shift < alphabet_letters.length) {
            String decrypted = "";
            int i_2 = 0;
            while (i_2 < _runeLen(ciphertext)) {
                String ch = _substr(ciphertext, i_2, i_2 + 1);
                int idx = index_of(((String[])(alphabet_letters)), ch.toLowerCase());
                if (idx >= 0) {
                    int m = alphabet_letters.length;
                    int new_idx = Math.floorMod((idx - shift), m);
                    if (new_idx < 0) {
                        new_idx = new_idx + m;
                    }
                    String new_char = alphabet_letters[new_idx];
                    if (((Boolean)(case_sensitive)) && !(ch.equals(ch.toLowerCase()))) {
                        decrypted = decrypted + new_char.toUpperCase();
                    } else {
                        decrypted = decrypted + new_char;
                    }
                } else {
                    decrypted = decrypted + ch;
                }
                i_2 = i_2 + 1;
            }
            double chi = 0.0;
            String lowered = String.valueOf(case_sensitive ? decrypted.toLowerCase() : decrypted);
            int j = 0;
            while (j < alphabet_letters.length) {
                String letter = alphabet_letters[j];
                int occ = count_char(lowered, letter);
                if (occ > 0) {
                    double occf = ((Number)(occ)).doubleValue();
                    double expected = (double)(((double)(frequencies).getOrDefault(letter, 0.0))) * occf;
                    double diff = occf - expected;
                    chi = chi + ((diff * diff) / expected) * occf;
                }
                j = j + 1;
            }
            if (shift == 0 || chi < best_chi) {
                best_shift = shift;
                best_chi = chi;
                best_text = decrypted;
            }
            shift = shift + 1;
        }
        return new Result(best_shift, best_chi, best_text);
    }
    public static void main(String[] args) {
        r1 = decrypt_caesar_with_chi_squared("dof pz aol jhlzhy jpwoly zv wvwbshy? pa pz avv lhzf av jyhjr!", ((String[])(new String[]{})), ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>())), false);
        System.out.println(_p(r1.shift) + ", " + _p(r1.chi) + ", " + r1.decoded);
        r2 = decrypt_caesar_with_chi_squared("crybd cdbsxq", ((String[])(new String[]{})), ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>())), false);
        System.out.println(_p(r2.shift) + ", " + _p(r2.chi) + ", " + r2.decoded);
        r3 = decrypt_caesar_with_chi_squared("Crybd Cdbsxq", ((String[])(new String[]{})), ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>())), true);
        System.out.println(_p(r3.shift) + ", " + _p(r3.chi) + ", " + r3.decoded);
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
