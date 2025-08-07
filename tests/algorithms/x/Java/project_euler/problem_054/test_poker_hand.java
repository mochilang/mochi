public class Main {
    static class Hand {
        int rank;
        int[] values;
        Hand(int rank, int[] values) {
            this.rank = rank;
            this.values = values;
        }
        Hand() {}
        @Override public String toString() {
            return String.format("{'rank': %s, 'values': %s}", String.valueOf(rank), String.valueOf(values));
        }
    }


    static String[] split(String s, String sep) {
        String[] parts = ((String[])(new String[]{}));
        String cur = "";
        int i = 0;
        while (i < _runeLen(s)) {
            if (_runeLen(sep) > 0 && i + _runeLen(sep) <= _runeLen(s) && (_substr(s, i, i + _runeLen(sep)).equals(sep))) {
                parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new)));
                cur = "";
                i = i + _runeLen(sep);
            } else {
                cur = cur + s.substring(i, i + 1);
                i = i + 1;
            }
        }
        parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new)));
        return parts;
    }

    static int card_value(String ch) {
        if ((ch.equals("A"))) {
            return 14;
        } else         if ((ch.equals("K"))) {
            return 13;
        } else         if ((ch.equals("Q"))) {
            return 12;
        } else         if ((ch.equals("J"))) {
            return 11;
        } else         if ((ch.equals("T"))) {
            return 10;
        } else         if ((ch.equals("9"))) {
            return 9;
        } else         if ((ch.equals("8"))) {
            return 8;
        } else         if ((ch.equals("7"))) {
            return 7;
        } else         if ((ch.equals("6"))) {
            return 6;
        } else         if ((ch.equals("5"))) {
            return 5;
        } else         if ((ch.equals("4"))) {
            return 4;
        } else         if ((ch.equals("3"))) {
            return 3;
        } else {
            return 2;
        }
    }

    static Hand parse_hand(String hand) {
        int[] counts = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 <= 14) {
            counts = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(counts), java.util.stream.IntStream.of(0)).toArray()));
            i_1 = i_1 + 1;
        }
        String[] suits = ((String[])(new String[]{}));
        for (var card : hand.split(java.util.regex.Pattern.quote(" "))) {
            int v = card_value((String)(card));
counts[v] = counts[v] + 1;
            suits = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(suits), java.util.stream.Stream.of(card)).toArray(String[]::new)));
        }
        int[] vals = ((int[])(new int[]{}));
        int v_1 = 14;
        while (v_1 >= 2) {
            int c = counts[v_1];
            int k = 0;
            while (k < c) {
                vals = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(vals), java.util.stream.IntStream.of(v_1)).toArray()));
                k = k + 1;
            }
            v_1 = v_1 - 1;
        }
        boolean is_straight = false;
        if (vals.length == 5 && vals[0] == 14 && vals[1] == 5 && vals[2] == 4 && vals[3] == 3 && vals[4] == 2) {
            is_straight = true;
vals[0] = 5;
vals[1] = 4;
vals[2] = 3;
vals[3] = 2;
vals[4] = 14;
        } else {
            is_straight = true;
            int j = 0;
            while (j < 4) {
                if (vals[j] - vals[j + 1] != 1) {
                    is_straight = false;
                }
                j = j + 1;
            }
        }
        boolean is_flush = true;
        String s0 = suits[0];
        int t = 1;
        while (t < suits.length) {
            if (!(suits[t].equals(s0))) {
                is_flush = false;
            }
            t = t + 1;
        }
        int four_val = 0;
        int three_val = 0;
        int[] pair_vals = ((int[])(new int[]{}));
        v_1 = 14;
        while (v_1 >= 2) {
            if (counts[v_1] == 4) {
                four_val = v_1;
            } else             if (counts[v_1] == 3) {
                three_val = v_1;
            } else             if (counts[v_1] == 2) {
                pair_vals = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(pair_vals), java.util.stream.IntStream.of(v_1)).toArray()));
            }
            v_1 = v_1 - 1;
        }
        int rank = 1;
        if (((Boolean)(is_flush)) && ((Boolean)(is_straight)) && vals[0] == 14 && vals[4] == 10) {
            rank = 10;
        } else         if (((Boolean)(is_flush)) && ((Boolean)(is_straight))) {
            rank = 9;
        } else         if (four_val != 0) {
            rank = 8;
        } else         if (three_val != 0 && pair_vals.length == 1) {
            rank = 7;
        } else         if (((Boolean)(is_flush))) {
            rank = 6;
        } else         if (((Boolean)(is_straight))) {
            rank = 5;
        } else         if (three_val != 0) {
            rank = 4;
        } else         if (pair_vals.length == 2) {
            rank = 3;
        } else         if (pair_vals.length == 1) {
            rank = 2;
        } else {
            rank = 1;
        }
        return new Hand(rank, vals);
    }

    static String compare(Hand a, Hand b) {
        if (a.rank > b.rank) {
            return "Win";
        }
        if (a.rank < b.rank) {
            return "Loss";
        }
        int i_2 = 0;
        while (i_2 < a.values.length) {
            if (a.values[i_2] > b.values[i_2]) {
                return "Win";
            }
            if (a.values[i_2] < b.values[i_2]) {
                return "Loss";
            }
            i_2 = i_2 + 1;
        }
        return "Tie";
    }

    static void main() {
        String[][] tests = ((String[][])(new String[][]{new String[]{"2H 3H 4H 5H 6H", "KS AS TS QS JS", "Loss"}, new String[]{"2H 3H 4H 5H 6H", "AS AD AC AH JD", "Win"}, new String[]{"AS AH 2H AD AC", "JS JD JC JH 3D", "Win"}, new String[]{"2S AH 2H AS AC", "JS JD JC JH AD", "Loss"}, new String[]{"2S AH 2H AS AC", "2H 3H 5H 6H 7H", "Win"}}));
        for (String[] t : tests) {
            String res = String.valueOf(compare(parse_hand(t[0]), parse_hand(t[1])));
            System.out.println(res + " expected " + t[2]);
        }
    }
    public static void main(String[] args) {
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
}
