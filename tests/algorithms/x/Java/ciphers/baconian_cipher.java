public class Main {
    static java.util.Map<String,String> encode_map;
    static java.util.Map<String,String> decode_map;

    static java.util.Map<String,String> make_decode_map() {
        java.util.Map<String,String> m = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>()));
        for (String k : encode_map.keySet()) {
m.put(((String)(encode_map).get(k)), (String)(k));
        }
        return m;
    }

    static String[] split_spaces(String s) {
        String[] parts = ((String[])(new String[]{}));
        String current = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = _substr(s, i, i + 1);
            if ((ch.equals(" "))) {
                parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                current = "";
            } else {
                current = current + ch;
            }
            i = i + 1;
        }
        parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        return parts;
    }

    static String encode(String word) {
        String w = word.toLowerCase();
        String encoded = "";
        int i_1 = 0;
        while (i_1 < _runeLen(w)) {
            String ch_1 = _substr(w, i_1, i_1 + 1);
            if (((Boolean)(encode_map.containsKey(ch_1)))) {
                encoded = encoded + ((String)(encode_map).get(ch_1));
            } else {
                throw new RuntimeException(String.valueOf("encode() accepts only letters of the alphabet and spaces"));
            }
            i_1 = i_1 + 1;
        }
        return encoded;
    }

    static String decode(String coded) {
        int i_2 = 0;
        while (i_2 < _runeLen(coded)) {
            String ch_2 = _substr(coded, i_2, i_2 + 1);
            if (!(ch_2.equals("A")) && !(ch_2.equals("B")) && !(ch_2.equals(" "))) {
                throw new RuntimeException(String.valueOf("decode() accepts only 'A', 'B' and spaces"));
            }
            i_2 = i_2 + 1;
        }
        String[] words = ((String[])(split_spaces(coded)));
        String decoded = "";
        int w_1 = 0;
        while (w_1 < words.length) {
            String word = words[w_1];
            int j = 0;
            while (j < _runeLen(word)) {
                String segment = _substr(word, j, j + 5);
                decoded = decoded + ((String)(decode_map).get(segment));
                j = j + 5;
            }
            if (w_1 < words.length - 1) {
                decoded = decoded + " ";
            }
            w_1 = w_1 + 1;
        }
        return decoded;
    }
    public static void main(String[] args) {
        encode_map = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("a", "AAAAA"), java.util.Map.entry("b", "AAAAB"), java.util.Map.entry("c", "AAABA"), java.util.Map.entry("d", "AAABB"), java.util.Map.entry("e", "AABAA"), java.util.Map.entry("f", "AABAB"), java.util.Map.entry("g", "AABBA"), java.util.Map.entry("h", "AABBB"), java.util.Map.entry("i", "ABAAA"), java.util.Map.entry("j", "BBBAA"), java.util.Map.entry("k", "ABAAB"), java.util.Map.entry("l", "ABABA"), java.util.Map.entry("m", "ABABB"), java.util.Map.entry("n", "ABBAA"), java.util.Map.entry("o", "ABBAB"), java.util.Map.entry("p", "ABBBA"), java.util.Map.entry("q", "ABBBB"), java.util.Map.entry("r", "BAAAA"), java.util.Map.entry("s", "BAAAB"), java.util.Map.entry("t", "BAABA"), java.util.Map.entry("u", "BAABB"), java.util.Map.entry("v", "BBBAB"), java.util.Map.entry("w", "BABAA"), java.util.Map.entry("x", "BABAB"), java.util.Map.entry("y", "BABBA"), java.util.Map.entry("z", "BABBB"), java.util.Map.entry(" ", " ")))));
        decode_map = make_decode_map();
        System.out.println(encode("hello"));
        System.out.println(encode("hello world"));
        System.out.println(decode("AABBBAABAAABABAABABAABBAB BABAAABBABBAAAAABABAAAABB"));
        System.out.println(decode("AABBBAABAAABABAABABAABBAB"));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
