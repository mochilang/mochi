public class Main {
    static int[] daysInMonth = new int[]{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    static int[] start = new int[]{3, 6, 6, 2, 4, 0, 2, 5, 1, 3, 6, 1};
    static String[] months = new String[]{" January ", " February", "  March  ", "  April  ", "   May   ", "   June  ", "   July  ", "  August ", "September", " October ", " November", " December"};
    static String[] days = new String[]{"Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"};
    static int qtr = 0;

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("                                [SNOOPY]\n");
            System.out.println("                                  1969\n");
            while (qtr < 4) {
                int mi = 0;
                while (mi < 3) {
                    System.out.println("      " + months[qtr * 3 + mi] + "           " + " " + String.valueOf(false ? "True" : "False"));
                    mi = mi + 1;
                }
                System.out.println("");
                mi = 0;
                while (mi < 3) {
                    int d = 0;
                    while (d < 7) {
                        System.out.println(" " + days[d] + " " + String.valueOf(false ? "True" : "False"));
                        d = d + 1;
                    }
                    System.out.println("     " + " " + String.valueOf(false ? "True" : "False"));
                    mi = mi + 1;
                }
                System.out.println("");
                int week = 0;
                while (week < 6) {
                    mi = 0;
                    while (mi < 3) {
                        int day = 0;
                        while (day < 7) {
                            int m = qtr * 3 + mi;
                            int val = week * 7 + day - start[m] + 1;
                            if (val >= 1 && val <= daysInMonth[m]) {
                                String s = String.valueOf(val);
                                if (s.length() == 1) {
                                    s = " " + s;
                                }
                                System.out.println(" " + s + " " + String.valueOf(false ? "True" : "False"));
                            } else {
                                System.out.println("   " + " " + String.valueOf(false ? "True" : "False"));
                            }
                            day = day + 1;
                        }
                        System.out.println("     " + " " + String.valueOf(false ? "True" : "False"));
                        mi = mi + 1;
                    }
                    System.out.println("");
                    week = week + 1;
                }
                System.out.println("");
                qtr = qtr + 1;
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
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
