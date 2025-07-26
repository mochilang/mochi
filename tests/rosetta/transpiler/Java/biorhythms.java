public class Main {
    static double PI = 3.141592653589793;
    static double TWO_PI = 6.283185307179586;

    static double sinApprox(double x) {
        double term = x;
        double sum = x;
        int n = 1;
        while (n <= 8) {
            double denom = ((Number)(((2 * n) * (2 * n + 1)))).doubleValue();
            term = -term * x * x / denom;
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double floor(double x) {
        int i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double absFloat(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static int absInt(int n) {
        if (n < 0) {
            return -n;
        }
        return n;
    }

    static int parseIntStr(String str) {
        int i = 0;
        boolean neg = false;
        if (str.length() > 0 && str.substring(0, 1) == "-") {
            neg = true;
            i = 1;
        }
        int n = 0;
        java.util.Map<String,Integer> digits = new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1), java.util.Map.entry("2", 2), java.util.Map.entry("3", 3), java.util.Map.entry("4", 4), java.util.Map.entry("5", 5), java.util.Map.entry("6", 6), java.util.Map.entry("7", 7), java.util.Map.entry("8", 8), java.util.Map.entry("9", 9)));
        while (i < str.length()) {
            n = n * 10 + (int)(((int)digits.getOrDefault(str.substring(i, i + 1), 0)));
            i = i + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }

    static int[] parseDate(String s) {
        int y = parseIntStr(s.substring(0, 4));
        int m = parseIntStr(s.substring(5, 7));
        int d = parseIntStr(s.substring(8, 10));
        return new int[]{y, m, d};
    }

    static boolean leap(int y) {
        if (((Number)(Math.floorMod(y, 400))).intValue() == 0) {
            return true;
        }
        if (((Number)(Math.floorMod(y, 100))).intValue() == 0) {
            return false;
        }
        return ((Number)(Math.floorMod(y, 4))).intValue() == 0;
    }

    static int daysInMonth(int y, int m) {
        int feb = leap(y) ? 29 : 28;
        int[] lengths = new int[]{31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
        return lengths[m - 1];
    }

    static int[] addDays(int y, int m, int d, int n) {
        int yy = y;
        int mm = m;
        int dd = d;
        if (n >= 0) {
            int i = 0;
            while (i < n) {
                dd = dd + 1;
                if (dd > daysInMonth(yy, mm)) {
                    dd = 1;
                    mm = mm + 1;
                    if (mm > 12) {
                        mm = 1;
                        yy = yy + 1;
                    }
                }
                i = i + 1;
            }
        } else {
            int i = 0;
            while (i > n) {
                dd = dd - 1;
                if (dd < 1) {
                    mm = mm - 1;
                    if (mm < 1) {
                        mm = 12;
                        yy = yy - 1;
                    }
                    dd = daysInMonth(yy, mm);
                }
                i = i - 1;
            }
        }
        return new int[]{yy, mm, dd};
    }

    static String pad2(int n) {
        if (n < 10) {
            return "0" + String.valueOf(n);
        }
        return String.valueOf(n);
    }

    static String dateString(int y, int m, int d) {
        return String.valueOf(y) + "-" + String.valueOf(pad2(m)) + "-" + String.valueOf(pad2(d));
    }

    static int day(int y, int m, int d) {
        int part1 = 367 * y;
        int part2 = ((Number)(((7 * (((Number)((y + ((m + 9) / 12)))).intValue())) / 4))).intValue();
        int part3 = ((Number)(((275 * m) / 9))).intValue();
        return part1 - part2 + part3 + d - 730530;
    }

    static void biorhythms(String birth, String target) {
        int[] bparts = parseDate(birth);
        int by = bparts[0];
        int bm = bparts[1];
        int bd = bparts[2];
        int[] tparts = parseDate(target);
        int ty = tparts[0];
        int tm = tparts[1];
        int td = tparts[2];
        int diff = absInt(day(ty, tm, td) - day(by, bm, bd));
        System.out.println("Born " + birth + ", Target " + target);
        System.out.println("Day " + String.valueOf(diff));
        String[] cycles = new String[]{"Physical day ", "Emotional day", "Mental day   "};
        int[] lengths = new int[]{23, 28, 33};
        String[][] quadrants = new String[][]{new String[]{"up and rising", "peak"}, new String[]{"up but falling", "transition"}, new String[]{"down and falling", "valley"}, new String[]{"down but rising", "transition"}};
        int i = 0;
        while (i < 3) {
            int length = lengths[i];
            String cycle = String.valueOf(cycles[i]);
            int position = ((Number)(Math.floorMod(diff, length))).intValue();
            int quadrant = (position * 4) / length;
            double percent = sinApprox(2.0 * PI * (((Number)(position)).doubleValue()) / (((Number)(length)).doubleValue()));
            percent = floor(percent * 1000.0) / 10.0;
            String description = "";
            if (percent > 95.0) {
                description = " peak";
            } else             if (percent < (-95.0)) {
                description = " valley";
            } else             if (absFloat(percent) < 5.0) {
                description = " critical transition";
            } else {
                int daysToAdd = (quadrant + 1) * length / 4 - position;
                int[] res = addDays(ty, tm, td, daysToAdd);
                int ny = res[0];
                int nm = res[1];
                int nd = res[2];
                String transition = String.valueOf(dateString(ny, nm, nd));
                String trend = String.valueOf(quadrants[quadrant][0]);
                String next = String.valueOf(quadrants[quadrant][1]);
                String pct = String.valueOf(percent);
                if (!pct.contains(".")) {
                    pct = pct + ".0";
                }
                description = " " + pct + "% (" + trend + ", next " + next + " " + transition + ")";
            }
            String posStr = String.valueOf(position);
            if (position < 10) {
                posStr = " " + posStr;
            }
            System.out.println(cycle + posStr + " : " + description);
            i = i + 1;
        }
        System.out.println("");
    }

    static void main() {
        String[][] pairs = new String[][]{new String[]{"1943-03-09", "1972-07-11"}, new String[]{"1809-01-12", "1863-11-19"}, new String[]{"1809-02-12", "1863-11-19"}};
        int idx = 0;
        while (idx < pairs.length) {
            String[] p = pairs[idx];
            biorhythms(String.valueOf(p[0]), String.valueOf(p[1]));
            idx = idx + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }
}
