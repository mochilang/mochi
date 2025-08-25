public class Main {
    static class WordSearch {
        String[] words;
        java.math.BigInteger width;
        java.math.BigInteger height;
        String[][] board;
        WordSearch(String[] words, java.math.BigInteger width, java.math.BigInteger height, String[][] board) {
            this.words = words;
            this.width = width;
            this.height = height;
            this.board = board;
        }
        WordSearch() {}
        @Override public String toString() {
            return String.format("{'words': %s, 'width': %s, 'height': %s, 'board': %s}", String.valueOf(words), String.valueOf(width), String.valueOf(height), String.valueOf(board));
        }
    }

    static java.math.BigInteger seed = java.math.BigInteger.valueOf(123456789);

    static java.math.BigInteger rand() {
        seed = new java.math.BigInteger(String.valueOf((seed.multiply(java.math.BigInteger.valueOf(1103515245)).add(java.math.BigInteger.valueOf(12345))).remainder(java.math.BigInteger.valueOf(2147483648L))));
        return new java.math.BigInteger(String.valueOf(seed));
    }

    static java.math.BigInteger rand_range(java.math.BigInteger max) {
        return new java.math.BigInteger(String.valueOf(rand().remainder(max)));
    }

    static java.math.BigInteger[] shuffle(java.math.BigInteger[] list_int) {
        java.math.BigInteger i = new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(list_int.length)).subtract(java.math.BigInteger.valueOf(1))));
        while (i.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            java.math.BigInteger j_1 = new java.math.BigInteger(String.valueOf(rand_range(new java.math.BigInteger(String.valueOf(i.add(java.math.BigInteger.valueOf(1)))))));
            java.math.BigInteger tmp_1 = new java.math.BigInteger(String.valueOf(list_int[_idx((list_int).length, ((java.math.BigInteger)(i)).longValue())]));
list_int[(int)(((java.math.BigInteger)(i)).longValue())] = new java.math.BigInteger(String.valueOf(list_int[_idx((list_int).length, ((java.math.BigInteger)(j_1)).longValue())]));
list_int[(int)(((java.math.BigInteger)(j_1)).longValue())] = new java.math.BigInteger(String.valueOf(tmp_1));
            i = new java.math.BigInteger(String.valueOf(i.subtract(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[])(list_int));
    }

    static String rand_letter() {
        String letters = "abcdefghijklmnopqrstuvwxyz";
        java.math.BigInteger i_2 = new java.math.BigInteger(String.valueOf(rand_range(java.math.BigInteger.valueOf(26))));
        return _substr(letters, (int)(((java.math.BigInteger)(i_2)).longValue()), (int)(((java.math.BigInteger)(i_2.add(java.math.BigInteger.valueOf(1)))).longValue()));
    }

    static WordSearch make_word_search(String[] words, java.math.BigInteger width, java.math.BigInteger height) {
        String[][] board = ((String[][])(new String[][]{}));
        java.math.BigInteger r_1 = java.math.BigInteger.valueOf(0);
        while (r_1.compareTo(height) < 0) {
            String[] row_1 = ((String[])(new String[]{}));
            java.math.BigInteger c_1 = java.math.BigInteger.valueOf(0);
            while (c_1.compareTo(width) < 0) {
                row_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of("")).toArray(String[]::new)));
                c_1 = new java.math.BigInteger(String.valueOf(c_1.add(java.math.BigInteger.valueOf(1))));
            }
            board = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(board), java.util.stream.Stream.of(new String[][]{((String[])(row_1))})).toArray(String[][]::new)));
            r_1 = new java.math.BigInteger(String.valueOf(r_1.add(java.math.BigInteger.valueOf(1))));
        }
        return new WordSearch(((String[])(words)), new java.math.BigInteger(String.valueOf(width)), new java.math.BigInteger(String.valueOf(height)), ((String[][])(board)));
    }

    static boolean insert_dir(WordSearch ws, String word, java.math.BigInteger dr, java.math.BigInteger dc, java.math.BigInteger[] rows, java.math.BigInteger[] cols) {
        java.math.BigInteger word_len = new java.math.BigInteger(String.valueOf(_runeLen(word)));
        java.math.BigInteger ri_1 = java.math.BigInteger.valueOf(0);
        while (ri_1.compareTo(new java.math.BigInteger(String.valueOf(rows.length))) < 0) {
            java.math.BigInteger row_3 = new java.math.BigInteger(String.valueOf(rows[_idx((rows).length, ((java.math.BigInteger)(ri_1)).longValue())]));
            java.math.BigInteger ci_1 = java.math.BigInteger.valueOf(0);
            while (ci_1.compareTo(new java.math.BigInteger(String.valueOf(cols.length))) < 0) {
                java.math.BigInteger col_1 = new java.math.BigInteger(String.valueOf(cols[_idx((cols).length, ((java.math.BigInteger)(ci_1)).longValue())]));
                java.math.BigInteger end_r_1 = new java.math.BigInteger(String.valueOf(row_3.add(dr.multiply((word_len.subtract(java.math.BigInteger.valueOf(1)))))));
                java.math.BigInteger end_c_1 = new java.math.BigInteger(String.valueOf(col_1.add(dc.multiply((word_len.subtract(java.math.BigInteger.valueOf(1)))))));
                if (end_r_1.compareTo(java.math.BigInteger.valueOf(0)) < 0 || end_r_1.compareTo(ws.height) >= 0 || end_c_1.compareTo(java.math.BigInteger.valueOf(0)) < 0 || end_c_1.compareTo(ws.width) >= 0) {
                    ci_1 = new java.math.BigInteger(String.valueOf(ci_1.add(java.math.BigInteger.valueOf(1))));
                    continue;
                }
                java.math.BigInteger k_1 = java.math.BigInteger.valueOf(0);
                boolean ok_1 = true;
                while (k_1.compareTo(word_len) < 0) {
                    java.math.BigInteger rr_1 = new java.math.BigInteger(String.valueOf(row_3.add(dr.multiply(k_1))));
                    java.math.BigInteger cc_1 = new java.math.BigInteger(String.valueOf(col_1.add(dc.multiply(k_1))));
                    if (!(ws.board[_idx((ws.board).length, ((java.math.BigInteger)(rr_1)).longValue())][_idx((ws.board[_idx((ws.board).length, ((java.math.BigInteger)(rr_1)).longValue())]).length, ((java.math.BigInteger)(cc_1)).longValue())].equals(""))) {
                        ok_1 = false;
                        break;
                    }
                    k_1 = new java.math.BigInteger(String.valueOf(k_1.add(java.math.BigInteger.valueOf(1))));
                }
                if (ok_1) {
                    k_1 = java.math.BigInteger.valueOf(0);
                    while (k_1.compareTo(word_len) < 0) {
                        java.math.BigInteger rr2_1 = new java.math.BigInteger(String.valueOf(row_3.add(dr.multiply(k_1))));
                        java.math.BigInteger cc2_1 = new java.math.BigInteger(String.valueOf(col_1.add(dc.multiply(k_1))));
                        String[] row_list_1 = ((String[])(ws.board[_idx((ws.board).length, ((java.math.BigInteger)(rr2_1)).longValue())]));
row_list_1[(int)(((java.math.BigInteger)(cc2_1)).longValue())] = _substr(word, (int)(((java.math.BigInteger)(k_1)).longValue()), (int)(((java.math.BigInteger)(k_1.add(java.math.BigInteger.valueOf(1)))).longValue()));
                        k_1 = new java.math.BigInteger(String.valueOf(k_1.add(java.math.BigInteger.valueOf(1))));
                    }
                    return true;
                }
                ci_1 = new java.math.BigInteger(String.valueOf(ci_1.add(java.math.BigInteger.valueOf(1))));
            }
            ri_1 = new java.math.BigInteger(String.valueOf(ri_1.add(java.math.BigInteger.valueOf(1))));
        }
        return false;
    }

    static void generate_board(WordSearch ws) {
        java.math.BigInteger[] dirs_r = ((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))}));
        java.math.BigInteger[] dirs_c_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))}));
        java.math.BigInteger i_4 = java.math.BigInteger.valueOf(0);
        while (i_4.compareTo(new java.math.BigInteger(String.valueOf(ws.words.length))) < 0) {
            String word_1 = ws.words[_idx((ws.words).length, ((java.math.BigInteger)(i_4)).longValue())];
            java.math.BigInteger[] rows_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger r_3 = java.math.BigInteger.valueOf(0);
            while (r_3.compareTo(ws.height) < 0) {
                rows_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(rows_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(r_3)))).toArray(java.math.BigInteger[]::new)));
                r_3 = new java.math.BigInteger(String.valueOf(r_3.add(java.math.BigInteger.valueOf(1))));
            }
            java.math.BigInteger[] cols_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger c_3 = java.math.BigInteger.valueOf(0);
            while (c_3.compareTo(ws.width) < 0) {
                cols_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(cols_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(c_3)))).toArray(java.math.BigInteger[]::new)));
                c_3 = new java.math.BigInteger(String.valueOf(c_3.add(java.math.BigInteger.valueOf(1))));
            }
            rows_1 = ((java.math.BigInteger[])(shuffle(((java.math.BigInteger[])(rows_1)))));
            cols_1 = ((java.math.BigInteger[])(shuffle(((java.math.BigInteger[])(cols_1)))));
            java.math.BigInteger d_1 = new java.math.BigInteger(String.valueOf(rand_range(java.math.BigInteger.valueOf(8))));
            insert_dir(ws, word_1, new java.math.BigInteger(String.valueOf(dirs_r[_idx((dirs_r).length, ((java.math.BigInteger)(d_1)).longValue())])), new java.math.BigInteger(String.valueOf(dirs_c_1[_idx((dirs_c_1).length, ((java.math.BigInteger)(d_1)).longValue())])), ((java.math.BigInteger[])(rows_1)), ((java.math.BigInteger[])(cols_1)));
            i_4 = new java.math.BigInteger(String.valueOf(i_4.add(java.math.BigInteger.valueOf(1))));
        }
    }

    static String visualise(WordSearch ws, boolean add_fake_chars) {
        String result = "";
        java.math.BigInteger r_5 = java.math.BigInteger.valueOf(0);
        while (r_5.compareTo(ws.height) < 0) {
            java.math.BigInteger c_5 = java.math.BigInteger.valueOf(0);
            while (c_5.compareTo(ws.width) < 0) {
                String ch_1 = ws.board[_idx((ws.board).length, ((java.math.BigInteger)(r_5)).longValue())][_idx((ws.board[_idx((ws.board).length, ((java.math.BigInteger)(r_5)).longValue())]).length, ((java.math.BigInteger)(c_5)).longValue())];
                if ((ch_1.equals(""))) {
                    if (add_fake_chars) {
                        ch_1 = String.valueOf(rand_letter());
                    } else {
                        ch_1 = "#";
                    }
                }
                result = result + ch_1 + " ";
                c_5 = new java.math.BigInteger(String.valueOf(c_5.add(java.math.BigInteger.valueOf(1))));
            }
            result = result + "\n";
            r_5 = new java.math.BigInteger(String.valueOf(r_5.add(java.math.BigInteger.valueOf(1))));
        }
        return result;
    }

    static void main() {
        String[] words = ((String[])(new String[]{"cat", "dog", "snake", "fish"}));
        WordSearch ws_1 = make_word_search(((String[])(words)), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(10));
        generate_board(ws_1);
        System.out.println(visualise(ws_1, true));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
