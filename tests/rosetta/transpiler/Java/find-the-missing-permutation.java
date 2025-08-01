public class Main {
    static String[] given;

    static int idx(String ch) {
        if ((ch.equals("A"))) {
            return 0;
        }
        if ((ch.equals("B"))) {
            return 1;
        }
        if ((ch.equals("C"))) {
            return 2;
        }
        return 3;
    }

    static void main() {
        String res = "";
        int i = 0;
        while (i < given[0].length()) {
            int[] counts = new int[]{0, 0, 0, 0};
            for (String p : given) {
                String ch = _substr(p, i, i + 1);
                int j = idx(ch);
counts[j] = counts[j] + 1;
            }
            int j_1 = 0;
            while (j_1 < 4) {
                if (Math.floorMod(counts[j_1], 2) == 1) {
                    if (j_1 == 0) {
                        res = res + "A";
                    } else                     if (j_1 == 1) {
                        res = res + "B";
                    } else                     if (j_1 == 2) {
                        res = res + "C";
                    } else {
                        res = res + "D";
                    }
                }
                j_1 = j_1 + 1;
            }
            i = i + 1;
        }
        System.out.println(res);
    }
    public static void main(String[] args) {
        given = new String[]{"ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB"};
        main();
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
