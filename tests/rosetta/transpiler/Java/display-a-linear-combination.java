public class Main {

    static String padRight(String s, int w) {
        String r = s;
        while (_runeLen(r) < w) {
            r = r + " ";
        }
        return r;
    }

    static String linearCombo(int[] c) {
        String out = "";
        int i = 0;
        while (i < c.length) {
            int n = c[i];
            if (n != 0) {
                String op = "";
                if (n < 0 && _runeLen(out) == 0) {
                    op = "-";
                } else                 if (n < 0) {
                    op = " - ";
                } else                 if (n > 0 && _runeLen(out) == 0) {
                    op = "";
                } else {
                    op = " + ";
                }
                int av = n;
                if (av < 0) {
                    av = -av;
                }
                String coeff = String.valueOf(av) + "*";
                if (av == 1) {
                    coeff = "";
                }
                out = out + op + coeff + "e(" + String.valueOf(i + 1) + ")";
            }
            i = i + 1;
        }
        if (_runeLen(out) == 0) {
            return "0";
        }
        return out;
    }

    static void main() {
        int[][] combos = new int[][]{new int[]{1, 2, 3}, new int[]{0, 1, 2, 3}, new int[]{1, 0, 3, 4}, new int[]{1, 2, 0}, new int[]{0, 0, 0}, new int[]{0}, new int[]{1, 1, 1}, new int[]{-1, -1, -1}, new int[]{-1, -2, 0, -3}, new int[]{-1}};
        int idx = 0;
        while (idx < combos.length) {
            int[] c = combos[idx];
            String t = "[";
            int j = 0;
            while (j < c.length) {
                t = t + String.valueOf(c[j]);
                if (j < c.length - 1) {
                    t = t + ", ";
                }
                j = j + 1;
            }
            t = t + "]";
            String lc = String.valueOf(linearCombo(c));
            System.out.println(String.valueOf(padRight(t, 15)) + "  ->  " + lc);
            idx = idx + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
