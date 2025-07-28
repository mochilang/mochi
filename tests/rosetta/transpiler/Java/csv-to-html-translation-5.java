public class Main {
    static String c = "Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n" + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n" + "The multitude,Who are you?\n" + "Brians mother,I'm his mother; that's who!\n" + "The multitude,Behold his mother! Behold his mother!";
    static String[][] rows = new String[][]{};

    static String[] split(String s, String sep) {
        String[] out = new String[]{};
        int start = 0;
        int i = 0;
        int n = _runeLen(sep);
        while (i <= _runeLen(s) - n) {
            if ((_substr(s, i, i + n).equals(sep))) {
                out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(_substr(s, start, i))).toArray(String[]::new);
                i = i + n;
                start = i;
            } else {
                i = i + 1;
            }
        }
        out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(_substr(s, start, _runeLen(s)))).toArray(String[]::new);
        return out;
    }

    static String htmlEscape(String s) {
        String out = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = _substr(s, i, i + 1);
            if ((ch.equals("&"))) {
                out = out + "&amp;";
            } else             if ((ch.equals("<"))) {
                out = out + "&lt;";
            } else             if ((ch.equals(">"))) {
                out = out + "&gt;";
            } else {
                out = out + ch;
            }
            i = i + 1;
        }
        return out;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            for (var line : c.split("\n")) {
                rows = appendObj(rows, line.split(","));
            }
            System.out.println("<table>");
            for (String[] row : rows) {
                String cells = "";
                for (String cell : row) {
                    cells = cells + "<td>" + String.valueOf(htmlEscape(cell)) + "</td>";
                }
                System.out.println("    <tr>" + cells + "</tr>");
            }
            System.out.println("</table>");
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
