public class Main {

    static void floyd(int n) {
        System.out.println("Floyd " + _p(n) + ":");
        int lowerLeftCorner = n * (n - 1) / 2 + 1;
        int lastInColumn = lowerLeftCorner;
        int lastInRow = 1;
        int i = 1;
        int row = 1;
        String line = "";
        while (row <= n) {
            int w = _runeLen(_p(lastInColumn));
            if (i < lastInRow) {
                line = line + String.valueOf(pad(_p(i), w)) + " ";
                lastInColumn = lastInColumn + 1;
            } else {
                line = line + String.valueOf(pad(_p(i), w));
                System.out.println(line);
                line = "";
                row = row + 1;
                lastInRow = lastInRow + row;
                lastInColumn = lowerLeftCorner;
            }
            i = i + 1;
        }
    }

    static String pad(String s, int w) {
        String t = s;
        while (_runeLen(t) < w) {
            t = " " + t;
        }
        return t;
    }
    public static void main(String[] args) {
        floyd(5);
        floyd(14);
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
