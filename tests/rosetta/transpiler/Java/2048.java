public class Main {
    static int SIZE = 4;
    static int[][] board = newBoard();
    static java.util.Map<String,Object> r = spawnTile(board);
    static boolean full = (boolean)((boolean)(r.get("full")));
    static int score = 0;

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int[][] newBoard() {
        int[][] b = new int[][]{};
        int y = 0;
        while (y < SIZE) {
            int[] row = new int[]{};
            int x = 0;
            while (x < SIZE) {
                row = java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray();
                x = x + 1;
            }
            b = java.util.stream.Stream.concat(java.util.Arrays.stream(b), java.util.stream.Stream.of(row)).toArray(int[][]::new);
            y = y + 1;
        }
        return b;
    }

    static java.util.Map<String,Object> spawnTile(int[][] b) {
        int[][] empty = new int[][]{};
        int y = 0;
        while (y < SIZE) {
            int x = 0;
            while (x < SIZE) {
                if (b[y][x] == 0) {
                    empty = java.util.stream.Stream.concat(java.util.Arrays.stream(empty), java.util.stream.Stream.of(new int[]{x, y})).toArray(int[][]::new);
                }
                x = x + 1;
            }
            y = y + 1;
        }
        if (empty.length == 0) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.of("board", b, "full", true));
        }
        int idx = _now() % empty.length;
        int[] cell = empty[idx];
        int val = 4;
        if (_now() % 10 < 9) {
            val = 2;
        }
b[cell[1]][cell[0]] = val;
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.of("board", b, "full", empty.length == 1));
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

    static void draw(int[][] b, int score) {
        System.out.println("Score: " + String.valueOf(score));
        int y = 0;
        while (y < SIZE) {
            System.out.println("+----+----+----+----+");
            String line = "|";
            int x = 0;
            while (x < SIZE) {
                int v = b[y][x];
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

    static java.util.Map<String,Object> slideLeft(int[] row) {
        int[] xs = new int[]{};
        int i = 0;
        while (i < row.length) {
            if (row[i] != 0) {
                xs = java.util.stream.IntStream.concat(java.util.Arrays.stream(xs), java.util.stream.IntStream.of(row[i])).toArray();
            }
            i = i + 1;
        }
        int[] res = new int[]{};
        int gain = 0;
        i = 0;
        while (i < xs.length) {
            if (i + 1 < xs.length && xs[i] == xs[i + 1]) {
                int v = xs[i] * 2;
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
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.of("row", res, "gain", gain));
    }

    static java.util.Map<String,Object> moveLeft(int[][] b, int score) {
        boolean moved = false;
        int y = 0;
        while (y < SIZE) {
            java.util.Map<String,Object> r = slideLeft(b[y]);
            int[] new_ = (int[])((int[])(r.get("row")));
            score = score + (int)((int)(r.get("gain")));
            int x = 0;
            while (x < SIZE) {
                if (b[y][x] != new_[x]) {
                    moved = true;
                }
b[y][x] = new_[x];
                x = x + 1;
            }
            y = y + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.of("board", b, "score", score, "moved", moved));
    }

    static java.util.Map<String,Object> moveRight(int[][] b, int score) {
        boolean moved = false;
        int y = 0;
        while (y < SIZE) {
            int[] rev = reverseRow(b[y]);
            java.util.Map<String,Object> r = slideLeft(rev);
            rev = (int[])((int[])(r.get("row")));
            score = score + (int)((int)(r.get("gain")));
            rev = reverseRow(rev);
            int x = 0;
            while (x < SIZE) {
                if (b[y][x] != rev[x]) {
                    moved = true;
                }
b[y][x] = rev[x];
                x = x + 1;
            }
            y = y + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.of("board", b, "score", score, "moved", moved));
    }

    static int[] getCol(int[][] b, int x) {
        int[] col = new int[]{};
        int y = 0;
        while (y < SIZE) {
            col = java.util.stream.IntStream.concat(java.util.Arrays.stream(col), java.util.stream.IntStream.of(b[y][x])).toArray();
            y = y + 1;
        }
        return col;
    }

    static void setCol(int[][] b, int x, int[] col) {
        int y = 0;
        while (y < SIZE) {
b[y][x] = col[y];
            y = y + 1;
        }
    }

    static java.util.Map<String,Object> moveUp(int[][] b, int score) {
        boolean moved = false;
        int x = 0;
        while (x < SIZE) {
            int[] col = getCol(b, x);
            java.util.Map<String,Object> r = slideLeft(col);
            int[] new_ = (int[])((int[])(r.get("row")));
            score = score + (int)((int)(r.get("gain")));
            int y = 0;
            while (y < SIZE) {
                if (b[y][x] != new_[y]) {
                    moved = true;
                }
b[y][x] = new_[y];
                y = y + 1;
            }
            x = x + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.of("board", b, "score", score, "moved", moved));
    }

    static java.util.Map<String,Object> moveDown(int[][] b, int score) {
        boolean moved = false;
        int x = 0;
        while (x < SIZE) {
            int[] col = reverseRow(getCol(b, x));
            java.util.Map<String,Object> r = slideLeft(col);
            col = (int[])((int[])(r.get("row")));
            score = score + (int)((int)(r.get("gain")));
            col = reverseRow(col);
            int y = 0;
            while (y < SIZE) {
                if (b[y][x] != col[y]) {
                    moved = true;
                }
b[y][x] = col[y];
                y = y + 1;
            }
            x = x + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.of("board", b, "score", score, "moved", moved));
    }

    static boolean hasMoves(int[][] b) {
        int y = 0;
        while (y < SIZE) {
            int x = 0;
            while (x < SIZE) {
                if (b[y][x] == 0) {
                    return true;
                }
                if (x + 1 < SIZE && b[y][x] == b[y][x + 1]) {
                    return true;
                }
                if (y + 1 < SIZE && b[y][x] == b[y + 1][x]) {
                    return true;
                }
                x = x + 1;
            }
            y = y + 1;
        }
        return false;
    }

    static boolean has2048(int[][] b) {
        int y = 0;
        while (y < SIZE) {
            int x = 0;
            while (x < SIZE) {
                if (b[y][x] >= 2048) {
                    return true;
                }
                x = x + 1;
            }
            y = y + 1;
        }
        return false;
    }
    public static void main(String[] args) {
        board = (int[][])((int[][])(r.get("board")));
        r = spawnTile(board);
        board = (int[][])((int[][])(r.get("board")));
        full = (boolean)((boolean)(r.get("full")));
        draw(board, score);
        while (true) {
            System.out.println("Move: ");
            String cmd = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            boolean moved = false;
            if (((cmd.equals("a")) || cmd.equals("A"))) {
                java.util.Map<String,Object> m = moveLeft(board, score);
                board = (int[][])((int[][])(m.get("board")));
                score = (int)((int)(m.get("score")));
                moved = (boolean)((boolean)(m.get("moved")));
            }
            if (((cmd.equals("d")) || cmd.equals("D"))) {
                java.util.Map<String,Object> m = moveRight(board, score);
                board = (int[][])((int[][])(m.get("board")));
                score = (int)((int)(m.get("score")));
                moved = (boolean)((boolean)(m.get("moved")));
            }
            if (((cmd.equals("w")) || cmd.equals("W"))) {
                java.util.Map<String,Object> m = moveUp(board, score);
                board = (int[][])((int[][])(m.get("board")));
                score = (int)((int)(m.get("score")));
                moved = (boolean)((boolean)(m.get("moved")));
            }
            if (((cmd.equals("s")) || cmd.equals("S"))) {
                java.util.Map<String,Object> m = moveDown(board, score);
                board = (int[][])((int[][])(m.get("board")));
                score = (int)((int)(m.get("score")));
                moved = (boolean)((boolean)(m.get("moved")));
            }
            if (((cmd.equals("q")) || cmd.equals("Q"))) {
                break;
            }
            if (moved) {
                java.util.Map<String,Object> r2 = spawnTile(board);
                board = (int[][])((int[][])(r2.get("board")));
                full = (boolean)((boolean)(r2.get("full")));
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
        return (int)System.currentTimeMillis();
    }
}
