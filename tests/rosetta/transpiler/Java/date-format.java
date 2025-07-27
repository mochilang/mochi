public class Main {

    static String pad2(int n) {
        if (n < 10) {
            return "0" + String.valueOf(n);
        }
        return String.valueOf(n);
    }

    static String weekdayName(int z) {
        String[] names = new String[]{"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"};
        return names[Math.floorMod((z + 4), 7)];
    }

    static void main() {
        int ts = ((Number)((_now() / 1000000000))).intValue();
        int days = ((Number)((ts / 86400))).intValue();
        int z = days + 719468;
        int era = ((Number)((z / 146097))).intValue();
        int doe = z - era * 146097;
        int yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / ((Number)(365)).intValue();
        int y = yoe + era * 400;
        int doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
        int mp = (5 * doy + 2) / ((Number)(153)).intValue();
        int d = ((Number)((doy - ((153 * mp + 2) / ((Number)(5)).intValue()) + 1))).intValue();
        int m = ((Number)((mp + 3))).intValue();
        if (m > 12) {
            y = y + 1;
            m = m - 12;
        }
        String iso = String.valueOf(y) + "-" + String.valueOf(pad2(m)) + "-" + String.valueOf(pad2(d));
        System.out.println(iso);
        String[] months = new String[]{"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};
        String line = String.valueOf(weekdayName(days)) + ", " + months[m - 1] + " " + String.valueOf(d) + ", " + String.valueOf(y);
        System.out.println(line);
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
