public class Main {

    static int pow2(int n) {
        int p = 1;
        int i = 0;
        while (i < n) {
            p = p * 2;
            i = i + 1;
        }
        return p;
    }

    static int btoi(boolean b) {
        if (b) {
            return 1;
        }
        return 0;
    }

    static String addNoCells(String cells) {
        String l = "O";
        String r = "O";
        if ((_substr(cells, 0, 1).equals("O"))) {
            l = ".";
        }
        if ((_substr(cells, _runeLen(cells) - 1, _runeLen(cells)).equals("O"))) {
            r = ".";
        }
        cells = l + cells + r;
        cells = l + cells + r;
        return cells;
    }

    static String step(String cells, int ruleVal) {
        String newCells = "";
        int i = 0;
        while (i < _runeLen(cells) - 2) {
            int bin = 0;
            int b = 2;
            int n = i;
            while (n < i + 3) {
                bin = bin + btoi((_substr(cells, n, n + 1).equals("O"))) * pow2(b);
                b = b - 1;
                n = n + 1;
            }
            String a = ".";
            if ((Math.floorMod((ruleVal / pow2(bin)), 2) == 1)) {
                a = "O";
            }
            newCells = newCells + a;
            i = i + 1;
        }
        return newCells;
    }

    static String repeat(String ch, int n) {
        String s = "";
        int i = 0;
        while (i < n) {
            s = s + ch;
            i = i + 1;
        }
        return s;
    }

    static void evolve(int l, int ruleVal) {
        System.out.println(" Rule #" + String.valueOf(ruleVal) + ":");
        String cells = "O";
        int x = 0;
        while (x < l) {
            cells = String.valueOf(addNoCells(cells));
            int width = 40 + (_runeLen(cells) / 2);
            String spaces = String.valueOf(repeat(" ", width - _runeLen(cells)));
            System.out.println(spaces + cells);
            cells = String.valueOf(step(cells, ruleVal));
            x = x + 1;
        }
    }

    static void main() {
        for (int r : new int[]{90, 30}) {
            evolve(25, r);
            System.out.println("");
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
