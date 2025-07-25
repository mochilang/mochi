public class Main {
    static int SIZE = 4;
    static class Board {
        int[][] cells;
        Board(int[][] cells) {
            this.cells = cells;
        }
        @Override public String toString() {
            return String.format("{'cells': %s}", String.valueOf(cells));
        }
    }

    static class SpawnResult {
        Board board;
        boolean full;
        SpawnResult(Board board, boolean full) {
            this.board = board;
            this.full = full;
        }
        @Override public String toString() {
            return String.format("{'board': %s, 'full': %s}", String.valueOf(board), String.valueOf(full));
        }
    }

    static class SlideResult {
        int[] row;
        int gain;
        SlideResult(int[] row, int gain) {
            this.row = row;
            this.gain = gain;
        }
        @Override public String toString() {
            return String.format("{'row': %s, 'gain': %s}", String.valueOf(row), String.valueOf(gain));
        }
    }

    static class MoveResult {
        Board board;
        int score;
        boolean moved;
        MoveResult(Board board, int score, boolean moved) {
            this.board = board;
            this.score = score;
            this.moved = moved;
        }
        @Override public String toString() {
            return String.format("{'board': %s, 'score': %s, 'moved': %s}", String.valueOf(board), String.valueOf(score), String.valueOf(moved));
        }
    }

    static Board board = newBoard();
    static SpawnResult r = spawnTile(board);
    static boolean full = r.full;
    static int score = 0;

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static Board newBoard() {
        int[][] b = new int[][]{};
        int y = 0;
        while (y < SIZE) {
            int[] row = new int[]{};
            int x = 0;
            while (x < SIZE) {
                row = java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray();
                x = x + 1;
            }
            b = appendObj(b, row);
            y = y + 1;
        }
        return new Board(b);
    }

    static SpawnResult spawnTile(Board b) {
        int[][] grid = b.cells;
        int[][] empty = new int[][]{};
        int y = 0;
        while (y < SIZE) {
            int x = 0;
            while (x < SIZE) {
                if (((Number)(grid[y][x])).intValue() == 0) {
                    empty = appendObj(empty, new int[]{x, y});
                }
                x = x + 1;
            }
            y = y + 1;
        }
        if (empty.length == 0) {
            return new SpawnResult(b, true);
        }
        int idx = _now() % empty.length;
        int[] cell = empty[idx];
        int val = 4;
        if (_now() % 10 < 9) {
            val = 2;
        }
grid[cell[1]][cell[0]] = val;
        return new SpawnResult(new Board(grid), empty.length == 1);
    }

    static String pad(int n) {
        String s = String.valueOf(n);
        int pad = 4 - s.length();
        int i = 0;
        String out = "";
        while (i < pad) {
            out = out + " ";
            i = i + 1;
        }
        return out + s;
    }

    static void draw(Board b, int score) {
        System.out.println("Score: " + String.valueOf(score));
        int y = 0;
        while (y < SIZE) {
            System.out.println("+----+----+----+----+");
            String line = "|";
            int x = 0;
            while (x < SIZE) {
                int v = b.cells[y][x];
                if (v == 0) {
                    line = line + "    |";
                } else {
                    line = line + pad(v) + "|";
                }
                x = x + 1;
            }
            System.out.println(line);
            y = y + 1;
        }
        System.out.println("+----+----+----+----+");
        System.out.println("W=Up S=Down A=Left D=Right Q=Quit");
    }

    static int[] reverseRow(int[] r) {
        int[] out = new int[]{};
        int i = r.length - 1;
        while (i >= 0) {
            out = java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(r[i])).toArray();
            i = i - 1;
        }
        return out;
    }

    static SlideResult slideLeft(int[] row) {
        int[] xs = new int[]{};
        int i = 0;
        while (i < row.length) {
            if (((Number)(row[i])).intValue() != 0) {
                xs = java.util.stream.IntStream.concat(java.util.Arrays.stream(xs), java.util.stream.IntStream.of(row[i])).toArray();
            }
            i = i + 1;
        }
        int[] res = new int[]{};
        int gain = 0;
        i = 0;
        while (i < xs.length) {
            if (i + 1 < xs.length && xs[i] == ((Number)(xs[i + 1])).intValue()) {
                int v = ((Number)(xs[i])).intValue() * 2;
                gain = gain + v;
                res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(v)).toArray();
                i = i + 2;
            } else {
                res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i])).toArray();
                i = i + 1;
            }
        }
        while (res.length < SIZE) {
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(0)).toArray();
        }
        return new SlideResult(res, gain);
    }

    static MoveResult moveLeft(Board b, int score) {
        int[][] grid = b.cells;
        boolean moved = false;
        int y = 0;
        while (y < SIZE) {
            SlideResult r = slideLeft(grid[y]);
            int[] new_ = r.row;
            score = score + ((Number)(r.gain)).intValue();
            int x = 0;
            while (x < SIZE) {
                if (((Number)(grid[y][x])).intValue() != ((Number)(new_[x])).intValue()) {
                    moved = true;
                }
grid[y][x] = new_[x];
                x = x + 1;
            }
            y = y + 1;
        }
        return new MoveResult(new Board(grid), score, moved);
    }

    static MoveResult moveRight(Board b, int score) {
        int[][] grid = b.cells;
        boolean moved = false;
        int y = 0;
        while (y < SIZE) {
            int[] rev = reverseRow(grid[y]);
            SlideResult r = slideLeft(rev);
            rev = r.row;
            score = score + ((Number)(r.gain)).intValue();
            rev = reverseRow(rev);
            int x = 0;
            while (x < SIZE) {
                if (((Number)(grid[y][x])).intValue() != ((Number)(rev[x])).intValue()) {
                    moved = true;
                }
grid[y][x] = rev[x];
                x = x + 1;
            }
            y = y + 1;
        }
        return new MoveResult(new Board(grid), score, moved);
    }

    static int[] getCol(Board b, int x) {
        int[] col = new int[]{};
        int y = 0;
        while (y < SIZE) {
            col = java.util.stream.IntStream.concat(java.util.Arrays.stream(col), java.util.stream.IntStream.of(b.cells[y][x])).toArray();
            y = y + 1;
        }
        return col;
    }

    static void setCol(Board b, int x, int[] col) {
        int[][] rows = b.cells;
        int y = 0;
        while (y < SIZE) {
            int[] row = rows[y];
row[x] = col[y];
rows[y] = row;
            y = y + 1;
        }
b.cells = rows;
    }

    static MoveResult moveUp(Board b, int score) {
        int[][] grid = b.cells;
        boolean moved = false;
        int x = 0;
        while (x < SIZE) {
            int[] col = getCol(b, x);
            SlideResult r = slideLeft(col);
            int[] new_ = r.row;
            score = score + ((Number)(r.gain)).intValue();
            int y = 0;
            while (y < SIZE) {
                if (((Number)(grid[y][x])).intValue() != ((Number)(new_[y])).intValue()) {
                    moved = true;
                }
grid[y][x] = new_[y];
                y = y + 1;
            }
            x = x + 1;
        }
        return new MoveResult(new Board(grid), score, moved);
    }

    static MoveResult moveDown(Board b, int score) {
        int[][] grid = b.cells;
        boolean moved = false;
        int x = 0;
        while (x < SIZE) {
            int[] col = reverseRow(getCol(b, x));
            SlideResult r = slideLeft(col);
            col = r.row;
            score = score + ((Number)(r.gain)).intValue();
            col = reverseRow(col);
            int y = 0;
            while (y < SIZE) {
                if (((Number)(grid[y][x])).intValue() != ((Number)(col[y])).intValue()) {
                    moved = true;
                }
grid[y][x] = col[y];
                y = y + 1;
            }
            x = x + 1;
        }
        return new MoveResult(new Board(grid), score, moved);
    }

    static boolean hasMoves(Board b) {
        int y = 0;
        while (y < SIZE) {
            int x = 0;
            while (x < SIZE) {
                if (((Number)(b.cells[y][x])).intValue() == 0) {
                    return true;
                }
                if (x + 1 < SIZE && b.cells[y][x] == ((Number)(b.cells[y][x + 1])).intValue()) {
                    return true;
                }
                if (y + 1 < SIZE && b.cells[y][x] == ((Number)(b.cells[y + 1][x])).intValue()) {
                    return true;
                }
                x = x + 1;
            }
            y = y + 1;
        }
        return false;
    }

    static boolean has2048(Board b) {
        int y = 0;
        while (y < SIZE) {
            int x = 0;
            while (x < SIZE) {
                if (((Number)(b.cells[y][x])).intValue() >= 2048) {
                    return true;
                }
                x = x + 1;
            }
            y = y + 1;
        }
        return false;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            board = r.board;
            r = spawnTile(board);
            board = r.board;
            full = r.full;
            draw(board, score);
            while (true) {
                System.out.println("Move: ");
                String cmd = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
                boolean moved = false;
                if (((cmd.equals("a")) || cmd.equals("A"))) {
                    MoveResult m = moveLeft(board, score);
                    board = m.board;
                    score = m.score;
                    moved = m.moved;
                }
                if (((cmd.equals("d")) || cmd.equals("D"))) {
                    MoveResult m = moveRight(board, score);
                    board = m.board;
                    score = m.score;
                    moved = m.moved;
                }
                if (((cmd.equals("w")) || cmd.equals("W"))) {
                    MoveResult m = moveUp(board, score);
                    board = m.board;
                    score = m.score;
                    moved = m.moved;
                }
                if (((cmd.equals("s")) || cmd.equals("S"))) {
                    MoveResult m = moveDown(board, score);
                    board = m.board;
                    score = m.score;
                    moved = m.moved;
                }
                if (((cmd.equals("q")) || cmd.equals("Q"))) {
                    break;
                }
                if (moved) {
                    SpawnResult r2 = spawnTile(board);
                    board = r2.board;
                    full = r2.full;
                    if (full && (!(Boolean)hasMoves(board))) {
                        draw(board, score);
                        System.out.println("Game Over");
                        break;
                    }
                }
                draw(board, score);
                if (has2048(board)) {
                    System.out.println("You win!");
                    break;
                }
                if (!(Boolean)hasMoves(board)) {
                    System.out.println("Game Over");
                    break;
                }
            }
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
        return rt.totalMemory() - rt.freeMemory();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
