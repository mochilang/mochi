public class Main {
    static class Birthday {
        int month;
        int day;
        Birthday(int month, int day) {
            this.month = month;
            this.day = day;
        }
        @Override public String toString() {
            return String.format("{'month': %s, 'day': %s}", String.valueOf(month), String.valueOf(day));
        }
    }

    static Birthday[] choices = new Birthday[]{new Birthday(5, 15), new Birthday(5, 16), new Birthday(5, 19), new Birthday(6, 17), new Birthday(6, 18), new Birthday(7, 14), new Birthday(7, 16), new Birthday(8, 14), new Birthday(8, 15), new Birthday(8, 17)};
    static Birthday[] filtered = new Birthday[]{};
    static Birthday[] filtered2 = new Birthday[]{};
    static Birthday[] filtered3 = new Birthday[]{};
    static Birthday[] filtered4 = new Birthday[]{};

    static boolean monthUnique(Birthday b, Birthday[] list) {
        int c = 0;
        for (Birthday x : list) {
            if (x.month == b.month) {
                c = c + 1;
            }
        }
        return c == 1;
    }

    static boolean dayUnique(Birthday b, Birthday[] list) {
        int c = 0;
        for (Birthday x : list) {
            if (x.day == b.day) {
                c = c + 1;
            }
        }
        return c == 1;
    }

    static boolean monthWithUniqueDay(Birthday b, Birthday[] list) {
        for (Birthday x : list) {
            if (x.month == b.month && dayUnique(x, list)) {
                return true;
            }
        }
        return false;
    }

    static String bstr(Birthday b) {
        String[] months = new String[]{"", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};
        return months[b.month] + " " + String.valueOf(b.day);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            for (Birthday bd : choices) {
                if (!(Boolean)monthUnique(bd, choices)) {
                    filtered = java.util.stream.Stream.concat(java.util.Arrays.stream(filtered), java.util.stream.Stream.of(bd)).toArray(Birthday[]::new);
                }
            }
            for (Birthday bd : filtered) {
                if (!(Boolean)monthWithUniqueDay(bd, filtered)) {
                    filtered2 = java.util.stream.Stream.concat(java.util.Arrays.stream(filtered2), java.util.stream.Stream.of(bd)).toArray(Birthday[]::new);
                }
            }
            for (Birthday bd : filtered2) {
                if (dayUnique(bd, filtered2)) {
                    filtered3 = java.util.stream.Stream.concat(java.util.Arrays.stream(filtered3), java.util.stream.Stream.of(bd)).toArray(Birthday[]::new);
                }
            }
            for (Birthday bd : filtered3) {
                if (monthUnique(bd, filtered3)) {
                    filtered4 = java.util.stream.Stream.concat(java.util.Arrays.stream(filtered4), java.util.stream.Stream.of(bd)).toArray(Birthday[]::new);
                }
            }
            if (filtered4.length == 1) {
                System.out.println("Cheryl's birthday is " + String.valueOf(bstr(filtered4[0])));
            } else {
                System.out.println("Something went wrong!");
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
