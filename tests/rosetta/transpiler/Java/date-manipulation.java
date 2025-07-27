public class Main {
    static java.util.Map<String,Integer> months = new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("January", 1), java.util.Map.entry("February", 2), java.util.Map.entry("March", 3), java.util.Map.entry("April", 4), java.util.Map.entry("May", 5), java.util.Map.entry("June", 6), java.util.Map.entry("July", 7), java.util.Map.entry("August", 8), java.util.Map.entry("September", 9), java.util.Map.entry("October", 10), java.util.Map.entry("November", 11), java.util.Map.entry("December", 12)));

    static boolean isLeap(int y) {
        if (Math.floorMod(y, 400) == 0) {
            return true;
        }
        if (Math.floorMod(y, 100) == 0) {
            return false;
        }
        return Math.floorMod(y, 4) == 0;
    }

    static int daysInMonth(int y, int m) {
        int feb = isLeap(y) ? 29 : 28;
        int[] lengths = new int[]{31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
        return lengths[m - 1];
    }

    static int daysBeforeYear(int y) {
        int days = 0;
        int yy = 1970;
        while (yy < y) {
            days = days + 365;
            if (isLeap(yy)) {
                days = days + 1;
            }
            yy = yy + 1;
        }
        return days;
    }

    static int daysBeforeMonth(int y, int m) {
        int days = 0;
        int mm = 1;
        while (mm < m) {
            days = days + daysInMonth(y, mm);
            mm = mm + 1;
        }
        return days;
    }

    static int epochSeconds(int y, int m, int d, int h, int mi) {
        int days = daysBeforeYear(y) + daysBeforeMonth(y, m) + (d - 1);
        return days * 86400 + h * 3600 + mi * 60;
    }

    static int[] fromEpoch(int sec) {
        int days = sec / 86400;
        int rem = Math.floorMod(sec, 86400);
        int y = 1970;
        while (true) {
            int dy = isLeap(y) ? 366 : 365;
            if (days >= dy) {
                days = days - dy;
                y = y + 1;
            } else {
                break;
            }
        }
        int m = 1;
        while (true) {
            int dim = daysInMonth(y, m);
            if (days >= dim) {
                days = days - dim;
                m = m + 1;
            } else {
                break;
            }
        }
        int d = days + 1;
        int h = rem / 3600;
        int mi = (Math.floorMod(rem, 3600)) / 60;
        return new int[]{y, m, d, h, mi};
    }

    static String pad2(int n) {
        if (n < 10) {
            return "0" + String.valueOf(n);
        }
        return String.valueOf(n);
    }

    static int absInt(int n) {
        if (n < 0) {
            return -n;
        }
        return n;
    }

    static String formatDate(int[] parts, int offset, String abbr) {
        int y = parts[0];
        int m = parts[1];
        int d = parts[2];
        int h = parts[3];
        int mi = parts[4];
        String sign = "+";
        if (offset < 0) {
            sign = "-";
        }
        int off = absInt(offset) / 60;
        String offh = String.valueOf(pad2(off / 60));
        String offm = String.valueOf(pad2(Math.floorMod(off, 60)));
        return String.valueOf(y) + "-" + String.valueOf(pad2(m)) + "-" + String.valueOf(pad2(d)) + " " + String.valueOf(pad2(h)) + ":" + String.valueOf(pad2(mi)) + ":00 " + sign + offh + offm + " " + abbr;
    }

    static int parseIntStr(String str) {
        int i = 0;
        boolean neg = false;
        if (str.length() > 0 && (str.substring(0, 1).equals("-"))) {
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

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < s.length()) {
            if ((s.substring(i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int[] parseTime(String s) {
        int c = ((Number)(s.indexOf(":"))).intValue();
        int h = Integer.parseInt(s.substring(0, c));
        int mi = Integer.parseInt(s.substring(c + 1, c + 3));
        String ampm = s.substring(s.length() - 2, s.length());
        int hh = h;
        if ((ampm.equals("pm")) && h != 12) {
            hh = h + 12;
        }
        if ((ampm.equals("am")) && h == 12) {
            hh = 0;
        }
        return new int[]{hh, mi};
    }

    static void main() {
        String input = "March 7 2009 7:30pm EST";
        System.out.println("Input:              " + input);
        Object parts = new Object[]{};
        String cur = "";
        int i = 0;
        while (i < input.length()) {
            String ch = input.substring(i, i + 1);
            if ((ch.equals(" "))) {
                if (cur.length() > 0) {
                    parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                    cur = "";
                }
            } else {
                cur = cur + ch;
            }
            i = i + 1;
        }
        if (cur.length() > 0) {
            parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        }
        int month = (int)(((int)months.getOrDefault(parts[0], 0)));
        int day = Integer.parseInt(parts[1]);
        int year = Integer.parseInt(parts[2]);
        int[] tm = parseTime(parts[3]);
        int hour = tm[0];
        int minute = tm[1];
        String tz = parts[4];
        java.util.Map<String,Integer> zoneOffsets = new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("EST", -18000), java.util.Map.entry("EDT", -14400), java.util.Map.entry("MST", -25200)));
        int local = epochSeconds(year, month, day, hour, minute);
        int utc = local - (int)(((int)zoneOffsets.getOrDefault(tz, 0)));
        int utc12 = utc + 43200;
        int startDST = epochSeconds(2009, 3, 8, 7, 0);
        int offEast = -18000;
        if (utc12 >= startDST) {
            offEast = -14400;
        }
        int[] eastParts = fromEpoch(utc12 + offEast);
        String eastAbbr = "EST";
        if (offEast == (-14400)) {
            eastAbbr = "EDT";
        }
        System.out.println("+12 hrs:            " + String.valueOf(formatDate(eastParts, offEast, eastAbbr)));
        int offAZ = -25200;
        int[] azParts = fromEpoch(utc12 + offAZ);
        System.out.println("+12 hrs in Arizona: " + String.valueOf(formatDate(azParts, offAZ, "MST")));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
