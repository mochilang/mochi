public class Main {
    static class Token {
        int offset;
        int length;
        String indicator;
        Token(int offset, int length, String indicator) {
            this.offset = offset;
            this.length = length;
            this.indicator = indicator;
        }
        Token() {}
        @Override public String toString() {
            return String.format("{'offset': %s, 'length': %s, 'indicator': '%s'}", String.valueOf(offset), String.valueOf(length), String.valueOf(indicator));
        }
    }

    static Token[] c1;
    static Token[] c2;
    static Token[] tokens_example;

    static String token_to_string(Token t) {
        return "(" + _p(t.offset) + ", " + _p(t.length) + ", " + t.indicator + ")";
    }

    static String tokens_to_string(Token[] ts) {
        String res = "[";
        int i = 0;
        while (i < ts.length) {
            res = res + String.valueOf(token_to_string(ts[i]));
            if (i < ts.length - 1) {
                res = res + ", ";
            }
            i = i + 1;
        }
        return res + "]";
    }

    static int match_length_from_index(String text, String window, int text_index, int window_index) {
        if (text_index >= _runeLen(text) || window_index >= _runeLen(window)) {
            return 0;
        }
        String tc = _substr(text, text_index, text_index + 1);
        String wc = _substr(window, window_index, window_index + 1);
        if (!(tc.equals(wc))) {
            return 0;
        }
        return 1 + match_length_from_index(text, window + tc, text_index + 1, window_index + 1);
    }

    static Token find_encoding_token(String text, String search_buffer) {
        if (_runeLen(text) == 0) {
            throw new RuntimeException(String.valueOf("We need some text to work with."));
        }
        int length = 0;
        int offset = 0;
        if (_runeLen(search_buffer) == 0) {
            return new Token(offset, length, _substr(text, 0, 1));
        }
        int i_1 = 0;
        while (i_1 < _runeLen(search_buffer)) {
            String ch = _substr(search_buffer, i_1, i_1 + 1);
            int found_offset = _runeLen(search_buffer) - i_1;
            if ((ch.equals(_substr(text, 0, 1)))) {
                int found_length = match_length_from_index(text, search_buffer, 0, i_1);
                if (found_length >= length) {
                    offset = found_offset;
                    length = found_length;
                }
            }
            i_1 = i_1 + 1;
        }
        return new Token(offset, length, _substr(text, length, length + 1));
    }

    static Token[] lz77_compress(String text, int window_size, int lookahead) {
        int search_buffer_size = window_size - lookahead;
        Token[] output = ((Token[])(new Token[]{}));
        String search_buffer = "";
        String remaining = text;
        while (_runeLen(remaining) > 0) {
            Token token = find_encoding_token(remaining, search_buffer);
            int add_len = token.length + 1;
            search_buffer = search_buffer + _substr(remaining, 0, add_len);
            if (_runeLen(search_buffer) > search_buffer_size) {
                search_buffer = _substr(search_buffer, _runeLen(search_buffer) - search_buffer_size, _runeLen(search_buffer));
            }
            remaining = _substr(remaining, add_len, _runeLen(remaining));
            output = ((Token[])(java.util.stream.Stream.concat(java.util.Arrays.stream(output), java.util.stream.Stream.of(token)).toArray(Token[]::new)));
        }
        return output;
    }

    static String lz77_decompress(Token[] tokens) {
        String output_1 = "";
        for (Token t : tokens) {
            int i_2 = 0;
            while (i_2 < t.length) {
                output_1 = output_1 + _substr(output_1, _runeLen(output_1) - t.offset, _runeLen(output_1) - t.offset + 1);
                i_2 = i_2 + 1;
            }
            output_1 = output_1 + t.indicator;
        }
        return output_1;
    }
    public static void main(String[] args) {
        c1 = ((Token[])(lz77_compress("ababcbababaa", 13, 6)));
        System.out.println(tokens_to_string(((Token[])(c1))));
        c2 = ((Token[])(lz77_compress("aacaacabcabaaac", 13, 6)));
        System.out.println(tokens_to_string(((Token[])(c2))));
        tokens_example = ((Token[])(new Token[]{new Token(0, 0, "c"), new Token(0, 0, "a"), new Token(0, 0, "b"), new Token(0, 0, "r"), new Token(3, 1, "c"), new Token(2, 1, "d"), new Token(7, 4, "r"), new Token(3, 5, "d")}));
        System.out.println(lz77_decompress(((Token[])(tokens_example))));
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
