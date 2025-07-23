public class Main {
    static int[] board = new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0};
    static int[] solved = new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0};
    static int empty = 15;
    static int moves = 0;
    static boolean quit = false;

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int randMove() {
        return _now() % 4;
    }

    static boolean isSolved() {
        int i = 0;
        while (i < 16) {
            if (board[i] != solved[i]) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static java.util.Map<String,Object> isValidMove(int m) {
        if (m == 0) {
            return new java.util.LinkedHashMap<String, Object>() {{ put("idx", empty - 4); put("ok", empty / 4 > 0); }};
        }
        if (m == 1) {
            return new java.util.LinkedHashMap<String, Object>() {{ put("idx", empty + 4); put("ok", empty / 4 < 3); }};
        }
        if (m == 2) {
            return new java.util.LinkedHashMap<String, Object>() {{ put("idx", empty + 1); put("ok", empty % 4 < 3); }};
        }
        if (m == 3) {
            return new java.util.LinkedHashMap<String, Object>() {{ put("idx", empty - 1); put("ok", empty % 4 > 0); }};
        }
        return new java.util.LinkedHashMap<String, Object>() {{ put("idx", 0); put("ok", false); }};
    }

    static boolean doMove(int m) {
        java.util.Map<String,Object> r = isValidMove(m);
        if (!(Boolean)r.get("ok")) {
            return false;
        }
        int i = empty;
        int j = ((Number)(r.get("idx"))).intValue();
        int tmp = board[i];
board[i] = board[j];
board[j] = tmp;
        empty = j;
        moves = moves + 1;
        return true;
    }

    static void shuffle(int n) {
        int i = 0;
        while (i < n || isSolved()) {
            if (doMove(randMove())) {
                i = i + 1;
            }
        }
    }

    static void printBoard() {
        String line = "";
        int i = 0;
        while (i < 16) {
            int val = board[i];
            if (val == 0) {
                line = line + "  .";
            } else {
                String s = String.valueOf(val);
                if (val < 10) {
                    line = line + "  " + s;
                } else {
                    line = line + " " + s;
                }
            }
            if (i % 4 == 3) {
                System.out.println(line);
                line = "";
            }
            i = i + 1;
        }
    }

    static void playOneMove() {
        while (true) {
            System.out.println("Enter move #" + String.valueOf(moves + 1) + " (U, D, L, R, or Q): ");
            String s = _scanner.nextLine();
            if ((s.equals(""))) {
                continue;
            }
            String c = s.substring(0, 1);
            int m = 0;
            if ((c.equals("U")) || c == "u") {
                m = 0;
            } else             if ((c.equals("D")) || c == "d") {
                m = 1;
            } else             if ((c.equals("R")) || c == "r") {
                m = 2;
            } else             if ((c.equals("L")) || c == "l") {
                m = 3;
            } else             if ((c.equals("Q")) || c == "q") {
                System.out.println("Quiting after " + String.valueOf(moves) + " moves.");
                quit = true;
                return;
            } else {
                System.out.println("Please enter \"U\", \"D\", \"L\", or \"R\" to move the empty cell\n" + "up, down, left, or right. You can also enter \"Q\" to quit.\n" + "Upper or lowercase is accepted and only the first non-blank\n" + "character is important (i.e. you may enter \"up\" if you like).");
                continue;
            }
            if (!(Boolean)doMove(m)) {
                System.out.println("That is not a valid move at the moment.");
                continue;
            }
            return;
        }
    }

    static void play() {
        System.out.println("Starting board:");
        while (!quit && isSolved() == false) {
            System.out.println("");
            printBoard();
            playOneMove();
        }
        if (isSolved()) {
            System.out.println("You solved the puzzle in " + String.valueOf(moves) + " moves.");
        }
    }

    static void main() {
        shuffle(50);
        play();
    }
    public static void main(String[] args) {
        main();
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
