public class Main {
    static class Token {
        java.math.BigInteger offset;
        java.math.BigInteger length;
        String indicator;
        Token(java.math.BigInteger offset, java.math.BigInteger length, String indicator) {
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
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(ts.length))) < 0) {
            res = res + String.valueOf(token_to_string(ts[(int)(((java.math.BigInteger)(i_1)).longValue())]));
            if (i_1.compareTo(new java.math.BigInteger(String.valueOf(ts.length)).subtract(java.math.BigInteger.valueOf(1))) < 0) {
                res = res + ", ";
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return res + "]";
    }

    static java.math.BigInteger match_length_from_index(String text, String window, java.math.BigInteger text_index, java.math.BigInteger window_index) {
        if (text_index.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(text)))) >= 0 || window_index.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(window)))) >= 0) {
            return java.math.BigInteger.valueOf(0);
        }
        String tc_1 = _substr(text, (int)(((java.math.BigInteger)(text_index)).longValue()), (int)(((java.math.BigInteger)(text_index.add(java.math.BigInteger.valueOf(1)))).longValue()));
        String wc_1 = _substr(window, (int)(((java.math.BigInteger)(window_index)).longValue()), (int)(((java.math.BigInteger)(window_index.add(java.math.BigInteger.valueOf(1)))).longValue()));
        if (!(tc_1.equals(wc_1))) {
            return java.math.BigInteger.valueOf(0);
        }
        return new java.math.BigInteger(String.valueOf(java.math.BigInteger.valueOf(1).add(match_length_from_index(text, window + tc_1, new java.math.BigInteger(String.valueOf(text_index.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(window_index.add(java.math.BigInteger.valueOf(1))))))));
    }

    static Token find_encoding_token(String text, String search_buffer) {
        if (new java.math.BigInteger(String.valueOf(_runeLen(text))).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            throw new RuntimeException(String.valueOf("We need some text to work with."));
        }
        java.math.BigInteger length_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger offset_1 = java.math.BigInteger.valueOf(0);
        if (new java.math.BigInteger(String.valueOf(_runeLen(search_buffer))).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return new Token(offset_1, length_1, _substr(text, (int)(0L), (int)(1L)));
        }
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(search_buffer)))) < 0) {
            String ch_1 = _substr(search_buffer, (int)(((java.math.BigInteger)(i_3)).longValue()), (int)(((java.math.BigInteger)(i_3.add(java.math.BigInteger.valueOf(1)))).longValue()));
            java.math.BigInteger found_offset_1 = new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(_runeLen(search_buffer))).subtract(i_3)));
            if ((ch_1.equals(_substr(text, (int)(0L), (int)(1L))))) {
                java.math.BigInteger found_length_1 = new java.math.BigInteger(String.valueOf(match_length_from_index(text, search_buffer, java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf(i_3)))));
                if (found_length_1.compareTo(length_1) >= 0) {
                    offset_1 = new java.math.BigInteger(String.valueOf(found_offset_1));
                    length_1 = new java.math.BigInteger(String.valueOf(found_length_1));
                }
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return new Token(offset_1, length_1, _substr(text, (int)(((java.math.BigInteger)(length_1)).longValue()), (int)(((java.math.BigInteger)(length_1.add(java.math.BigInteger.valueOf(1)))).longValue())));
    }

    static Token[] lz77_compress(String text, java.math.BigInteger window_size, java.math.BigInteger lookahead) {
        java.math.BigInteger search_buffer_size = new java.math.BigInteger(String.valueOf(window_size.subtract(lookahead)));
        Token[] output_1 = ((Token[])(new Token[]{}));
        String search_buffer_1 = "";
        String remaining_1 = text;
        while (new java.math.BigInteger(String.valueOf(_runeLen(remaining_1))).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            Token token_1 = find_encoding_token(remaining_1, search_buffer_1);
            java.math.BigInteger add_len_1 = new java.math.BigInteger(String.valueOf(token_1.length.add(java.math.BigInteger.valueOf(1))));
            search_buffer_1 = search_buffer_1 + _substr(remaining_1, (int)(0L), (int)(((java.math.BigInteger)(add_len_1)).longValue()));
            if (new java.math.BigInteger(String.valueOf(_runeLen(search_buffer_1))).compareTo(search_buffer_size) > 0) {
                search_buffer_1 = _substr(search_buffer_1, (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(_runeLen(search_buffer_1))).subtract(search_buffer_size))).longValue()), (int)((long)(_runeLen(search_buffer_1))));
            }
            remaining_1 = _substr(remaining_1, (int)(((java.math.BigInteger)(add_len_1)).longValue()), (int)((long)(_runeLen(remaining_1))));
            output_1 = ((Token[])(java.util.stream.Stream.concat(java.util.Arrays.stream(output_1), java.util.stream.Stream.of(token_1)).toArray(Token[]::new)));
        }
        return ((Token[])(output_1));
    }

    static String lz77_decompress(Token[] tokens) {
        String output_2 = "";
        for (Token t : tokens) {
            java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
            while (i_5.compareTo(t.length) < 0) {
                output_2 = output_2 + _substr(output_2, (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(_runeLen(output_2))).subtract(t.offset))).longValue()), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(_runeLen(output_2))).subtract(t.offset).add(java.math.BigInteger.valueOf(1)))).longValue()));
                i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
            }
            output_2 = output_2 + t.indicator;
        }
        return output_2;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            c1 = ((Token[])(lz77_compress("ababcbababaa", java.math.BigInteger.valueOf(13), java.math.BigInteger.valueOf(6))));
            System.out.println(tokens_to_string(((Token[])(c1))));
            c2 = ((Token[])(lz77_compress("aacaacabcabaaac", java.math.BigInteger.valueOf(13), java.math.BigInteger.valueOf(6))));
            System.out.println(tokens_to_string(((Token[])(c2))));
            tokens_example = ((Token[])(new Token[]{new Token(0, 0, "c"), new Token(0, 0, "a"), new Token(0, 0, "b"), new Token(0, 0, "r"), new Token(3, 1, "c"), new Token(2, 1, "d"), new Token(7, 4, "r"), new Token(3, 5, "d")}));
            System.out.println(lz77_decompress(((Token[])(tokens_example))));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
