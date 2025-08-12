public class Main {
    static class WordSearch {
        String[] words;
        long width;
        long height;
        String[][] board;
        WordSearch(String[] words, long width, long height, String[][] board) {
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

    static long seed = 0;

    static long rand() {
        seed = (long)(((long)(Math.floorMod(((long)(((long)((long)(seed) * (long)(1103515245)) + (long)(12345)))), 2147483648L))));
        return seed;
    }

    static long rand_range(long max) {
        return Math.floorMod(rand(), max);
    }

    static long[] shuffle(long[] list_int) {
        long i = (long)((long)(list_int.length) - (long)(1));
        while ((long)(i) > (long)(0)) {
            long j_1 = rand_range((long)((long)(i) + (long)(1)));
            long tmp_1 = list_int[(int)((long)(i))];
list_int[(int)((long)(i))] = list_int[(int)((long)(j_1))];
list_int[(int)((long)(j_1))] = tmp_1;
            i = (long)((long)(i) - (long)(1));
        }
        return list_int;
    }

    static String rand_letter() {
        String letters = "abcdefghijklmnopqrstuvwxyz";
        long i_2 = rand_range(26L);
        return _substr(letters, (int)((long)(i_2)), (int)((long)(i_2 + (long)(1)))));
    }

    static WordSearch make_word_search(String[] words, long width, long height) {
        String[][] board = ((String[][])(new String[][]{}));
        long r_1 = 0L;
        while ((long)(r_1) < height) {
            String[] row_1 = ((String[])(new String[]{}));
            long c_1 = 0L;
            while ((long)(c_1) < width) {
                row_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of("")).toArray(String[]::new)));
                c_1 = (long)((long)(c_1) + (long)(1));
            }
            board = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(board), java.util.stream.Stream.of(row_1)).toArray(String[][]::new)));
            r_1 = (long)((long)(r_1) + (long)(1));
        }
        return new WordSearch(words, width, height, board);
    }

    static boolean insert_dir(WordSearch ws, String word, long dr, long dc, long[] rows, long[] cols) {
        long word_len = (long)(_runeLen(word));
        long ri_1 = 0L;
        while ((long)(ri_1) < (long)(rows.length)) {
            long row_3 = rows[(int)((long)(ri_1))];
            long ci_1 = 0L;
            while ((long)(ci_1) < (long)(cols.length)) {
                long col_1 = cols[(int)((long)(ci_1))];
                long end_r_1 = (long)(row_3 + (long)(dr * (long)(((long)(word_len) - (long)(1)))));
                long end_c_1 = (long)(col_1 + (long)(dc * (long)(((long)(word_len) - (long)(1)))));
                if ((long)(end_r_1) < (long)(0) || (long)(end_r_1) >= (long)(ws.height) || (long)(end_c_1) < (long)(0) || (long)(end_c_1) >= (long)(ws.width)) {
                    ci_1 = (long)((long)(ci_1) + (long)(1));
                    continue;
                }
                long k_1 = 0L;
                boolean ok_1 = true;
                while ((long)(k_1) < (long)(word_len)) {
                    long rr_1 = (long)(row_3 + (long)(dr * (long)(k_1)));
                    long cc_1 = (long)(col_1 + (long)(dc * (long)(k_1)));
                    if (!(ws.board[(int)((long)(rr_1))][(int)((long)(cc_1))].equals(""))) {
                        ok_1 = false;
                        break;
                    }
                    k_1 = (long)((long)(k_1) + (long)(1));
                }
                if (ok_1) {
                    k_1 = (long)(0);
                    while ((long)(k_1) < (long)(word_len)) {
                        long rr2_1 = (long)(row_3 + (long)(dr * (long)(k_1)));
                        long cc2_1 = (long)(col_1 + (long)(dc * (long)(k_1)));
                        String[] row_list_1 = ((String[])(ws.board[(int)((long)(rr2_1))]));
row_list_1[(int)((long)(cc2_1))] = _substr(word, (int)((long)(k_1)), (int)((long)((long)(k_1) + (long)(1)))));
                        k_1 = (long)((long)(k_1) + (long)(1));
                    }
                    return true;
                }
                ci_1 = (long)((long)(ci_1) + (long)(1));
            }
            ri_1 = (long)((long)(ri_1) + (long)(1));
        }
        return false;
    }

    static void generate_board(WordSearch ws) {
        long[] dirs_r = ((long[])(new long[]{-1, -1, 0, 1, 1, 1, 0, -1}));
        long[] dirs_c_1 = ((long[])(new long[]{0, 1, 1, 1, 0, -1, -1, -1}));
        long i_4 = 0L;
        while ((long)(i_4) < (long)(ws.words.length)) {
            String word_1 = ws.words[(int)((long)(i_4))];
            long[] rows_1 = ((long[])(new long[]{}));
            long r_3 = 0L;
            while ((long)(r_3) < (long)(ws.height)) {
                rows_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(rows_1), java.util.stream.LongStream.of((long)(r_3))).toArray()));
                r_3 = (long)((long)(r_3) + (long)(1));
            }
            long[] cols_1 = ((long[])(new long[]{}));
            long c_3 = 0L;
            while ((long)(c_3) < (long)(ws.width)) {
                cols_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(cols_1), java.util.stream.LongStream.of((long)(c_3))).toArray()));
                c_3 = (long)((long)(c_3) + (long)(1));
            }
            rows_1 = ((long[])(shuffle(((long[])(rows_1)))));
            cols_1 = ((long[])(shuffle(((long[])(cols_1)))));
            long d_1 = rand_range(8L);
            insert_dir(ws, word_1, (long)(dirs_r[(int)((long)(d_1))]), (long)(dirs_c_1[(int)((long)(d_1))]), ((long[])(rows_1)), ((long[])(cols_1)));
            i_4 = (long)((long)(i_4) + (long)(1));
        }
    }

    static String visualise(WordSearch ws, boolean add_fake_chars) {
        String result = "";
        long r_5 = 0L;
        while ((long)(r_5) < (long)(ws.height)) {
            long c_5 = 0L;
            while ((long)(c_5) < (long)(ws.width)) {
                String ch_1 = ws.board[(int)((long)(r_5))][(int)((long)(c_5))];
                if ((ch_1.equals(""))) {
                    if (add_fake_chars) {
                        ch_1 = String.valueOf(rand_letter());
                    } else {
                        ch_1 = "#";
                    }
                }
                result = result + ch_1 + " ";
                c_5 = (long)((long)(c_5) + (long)(1));
            }
            result = result + "\n";
            r_5 = (long)((long)(r_5) + (long)(1));
        }
        return result;
    }

    static void main() {
        String[] words = ((String[])(new String[]{"cat", "dog", "snake", "fish"}));
        WordSearch ws_1 = make_word_search(((String[])(words)), 10L, 10L);
        generate_board(ws_1);
        System.out.println(visualise(ws_1, true));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = (long)(123456789);
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
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
}
