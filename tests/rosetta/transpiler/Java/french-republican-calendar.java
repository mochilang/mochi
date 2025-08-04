public class Main {
    static String[] gregorianStr;
    static int[] gregorian;
    static String[] republicanStr;
    static String[] sansculotidesStr;
    static int[] rep;
    static int[] gre;

    static boolean greLeap(int year) {
        int a = ((Number)((Math.floorMod(year, 4)))).intValue();
        int b = ((Number)((Math.floorMod(year, 100)))).intValue();
        int c = ((Number)((Math.floorMod(year, 400)))).intValue();
        return a == 0 && (b != 0 || c == 0);
    }

    static boolean repLeap(int year) {
        int a_1 = ((Number)((Math.floorMod((year + 1), 4)))).intValue();
        int b_1 = ((Number)((Math.floorMod((year + 1), 100)))).intValue();
        int c_1 = ((Number)((Math.floorMod((year + 1), 400)))).intValue();
        return a_1 == 0 && (b_1 != 0 || c_1 == 0);
    }

    static int greToDay(int d, int m, int y) {
        int yy = y;
        int mm = m;
        if (mm < 3) {
            yy = yy - 1;
            mm = mm + 12;
        }
        return yy * 36525 / 100 - yy / 100 + yy / 400 + 306 * (mm + 1) / 10 + d - 654842;
    }

    static int repToDay(int d, int m, int y) {
        int dd = d;
        int mm_1 = m;
        if (mm_1 == 13) {
            mm_1 = mm_1 - 1;
            dd = dd + 30;
        }
        if (((Boolean)(repLeap(y)))) {
            dd = dd - 1;
        }
        return 365 * y + (y + 1) / 4 - (y + 1) / 100 + (y + 1) / 400 + 30 * mm_1 + dd - 395;
    }

    static int[] dayToGre(int day) {
        int y = day * 100 / 36525;
        int d = day - y * 36525 / 100 + 21;
        y = y + 1792;
        d = d + y / 100 - y / 400 - 13;
        int m = 8;
        while (d > gregorian[m]) {
            d = d - gregorian[m];
            m = m + 1;
            if (m == 12) {
                m = 0;
                y = y + 1;
                if (((Boolean)(greLeap(y)))) {
gregorian[1] = 29;
                } else {
gregorian[1] = 28;
                }
            }
        }
        m = m + 1;
        return new int[]{d, m, y};
    }

    static int[] dayToRep(int day) {
        int y_1 = (day - 1) * 100 / 36525;
        if (((Boolean)(repLeap(y_1)))) {
            y_1 = y_1 - 1;
        }
        int d_1 = day - (y_1 + 1) * 36525 / 100 + 365 + (y_1 + 1) / 100 - (y_1 + 1) / 400;
        y_1 = y_1 + 1;
        int m_1 = 1;
        int sc = 5;
        if (((Boolean)(repLeap(y_1)))) {
            sc = 6;
        }
        while (d_1 > 30) {
            d_1 = d_1 - 30;
            m_1 = m_1 + 1;
            if (m_1 == 13) {
                if (d_1 > sc) {
                    d_1 = d_1 - sc;
                    m_1 = 1;
                    y_1 = y_1 + 1;
                    sc = 5;
                    if (((Boolean)(repLeap(y_1)))) {
                        sc = 6;
                    }
                }
            }
        }
        return new int[]{d_1, m_1, y_1};
    }

    static String formatRep(int d, int m, int y) {
        if (m == 13) {
            return sansculotidesStr[d - 1] + " " + _p(y);
        }
        return _p(d) + " " + republicanStr[m - 1] + " " + _p(y);
    }

    static String formatGre(int d, int m, int y) {
        return _p(d) + " " + gregorianStr[m - 1] + " " + _p(y);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            gregorianStr = ((String[])(new String[]{"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"}));
            gregorian = ((int[])(new int[]{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}));
            republicanStr = ((String[])(new String[]{"Vendemiaire", "Brumaire", "Frimaire", "Nivose", "Pluviose", "Ventose", "Germinal", "Floreal", "Prairial", "Messidor", "Thermidor", "Fructidor"}));
            sansculotidesStr = ((String[])(new String[]{"Fete de la vertu", "Fete du genie", "Fete du travail", "Fete de l'opinion", "Fete des recompenses", "Fete de la Revolution"}));
            rep = ((int[])(dayToRep(greToDay(20, 5, 1795))));
            System.out.println(formatRep(rep[0], rep[1], rep[2]));
            gre = ((int[])(dayToGre(repToDay(1, 9, 3))));
            System.out.println(formatGre(gre[0], gre[1], gre[2]));
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
