public class Main {
    static class WordSearch {
        String[] words;
        int width;
        int height;
        String[][] board;
        WordSearch(String[] words, int width, int height, String[][] board) {
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

    static int seed = 0;

    static int rand() {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static int rand_range(int max) {
        return Math.floorMod(rand(), max);
    }

    static int[] shuffle(int[] list_int) {
        int i = list_int.length - 1;
        while (i > 0) {
            int j = rand_range(i + 1);
            int tmp = list_int[i];
list_int[i] = list_int[j];
list_int[j] = tmp;
            i = i - 1;
        }
        return list_int;
    }

    static String rand_letter() {
        String letters = "abcdefghijklmnopqrstuvwxyz";
        int i_1 = rand_range(26);
        return letters.substring(i_1, i_1 + 1);
    }

    static WordSearch make_word_search(String[] words, int width, int height) {
        String[][] board = ((String[][])(new String[][]{}));
        int r = 0;
        while (r < height) {
            String[] row = ((String[])(new String[]{}));
            int c = 0;
            while (c < width) {
                row = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of("")).toArray(String[]::new)));
                c = c + 1;
            }
            board = ((String[][])(appendObj(board, row)));
            r = r + 1;
        }
        return new WordSearch(words, width, height, board);
    }

    static boolean insert_dir(WordSearch ws, String word, int dr, int dc, int[] rows, int[] cols) {
        int word_len = _runeLen(word);
        int ri = 0;
        while (ri < rows.length) {
            int row_1 = rows[ri];
            int ci = 0;
            while (ci < cols.length) {
                int col = cols[ci];
                int end_r = row_1 + dr * (word_len - 1);
                int end_c = col + dc * (word_len - 1);
                if (end_r < 0 || end_r >= ws.height || end_c < 0 || end_c >= ws.width) {
                    ci = ci + 1;
                    continue;
                }
                int k = 0;
                boolean ok = true;
                while (k < word_len) {
                    int rr = row_1 + dr * k;
                    int cc = col + dc * k;
                    if (!(ws.board[rr][cc].equals(""))) {
                        ok = false;
                        break;
                    }
                    k = k + 1;
                }
                if (ok) {
                    k = 0;
                    while (k < word_len) {
                        int rr2 = row_1 + dr * k;
                        int cc2 = col + dc * k;
                        String[] row_list = ((String[])(ws.board[rr2]));
row_list[cc2] = word.substring(k, k + 1);
                        k = k + 1;
                    }
                    return true;
                }
                ci = ci + 1;
            }
            ri = ri + 1;
        }
        return false;
    }

    static void generate_board(WordSearch ws) {
        int[] dirs_r = ((int[])(new int[]{-1, -1, 0, 1, 1, 1, 0, -1}));
        int[] dirs_c = ((int[])(new int[]{0, 1, 1, 1, 0, -1, -1, -1}));
        int i_2 = 0;
        while (i_2 < ws.words.length) {
            String word = ws.words[i_2];
            int[] rows = ((int[])(new int[]{}));
            int r_1 = 0;
            while (r_1 < ws.height) {
                rows = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(rows), java.util.stream.IntStream.of(r_1)).toArray()));
                r_1 = r_1 + 1;
            }
            int[] cols = ((int[])(new int[]{}));
            int c_1 = 0;
            while (c_1 < ws.width) {
                cols = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(cols), java.util.stream.IntStream.of(c_1)).toArray()));
                c_1 = c_1 + 1;
            }
            rows = ((int[])(shuffle(((int[])(rows)))));
            cols = ((int[])(shuffle(((int[])(cols)))));
            int d = rand_range(8);
            insert_dir(ws, word, dirs_r[d], dirs_c[d], ((int[])(rows)), ((int[])(cols)));
            i_2 = i_2 + 1;
        }
    }

    static String visualise(WordSearch ws, boolean add_fake_chars) {
        String result = "";
        int r_2 = 0;
        while (r_2 < ws.height) {
            int c_2 = 0;
            while (c_2 < ws.width) {
                String ch = ws.board[r_2][c_2];
                if ((ch.equals(""))) {
                    if (((Boolean)(add_fake_chars))) {
                        ch = String.valueOf(rand_letter());
                    } else {
                        ch = "#";
                    }
                }
                result = result + ch + " ";
                c_2 = c_2 + 1;
            }
            result = result + "\n";
            r_2 = r_2 + 1;
        }
        return result;
    }

    static void main() {
        String[] words = ((String[])(new String[]{"cat", "dog", "snake", "fish"}));
        WordSearch ws = make_word_search(((String[])(words)), 10, 10);
        generate_board(ws);
        System.out.println(visualise(ws, true));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = 123456789;
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
