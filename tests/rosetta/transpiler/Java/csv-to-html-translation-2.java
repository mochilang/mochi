public class Main {
    static String c = "Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n" + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n" + "The multitude,Who are you?\n" + "Brians mother,I'm his mother; that's who!\n" + "The multitude,Behold his mother! Behold his mother!";
    static String[][] rows = new String[][]{};
    static boolean headings = true;

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            for (var line : c.split("\n")) {
                rows = appendObj(rows, line.split(","));
            }
            System.out.println("<table>");
            if (headings) {
                if (rows.length > 0) {
                    String th = "";
                    for (String h : rows[0]) {
                        th = th + "<th>" + h + "</th>";
                    }
                    System.out.println("   <thead>");
                    System.out.println("      <tr>" + th + "</tr>");
                    System.out.println("   </thead>");
                    System.out.println("   <tbody>");
                    int i = 1;
                    while (i < rows.length) {
                        String cells = "";
                        for (String cell : rows[i]) {
                            cells = cells + "<td>" + cell + "</td>";
                        }
                        System.out.println("      <tr>" + cells + "</tr>");
                        i = i + 1;
                    }
                    System.out.println("   </tbody>");
                }
            } else {
                for (String[] row : rows) {
                    String cells = "";
                    for (String cell : row) {
                        cells = cells + "<td>" + cell + "</td>";
                    }
                    System.out.println("    <tr>" + cells + "</tr>");
                }
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
}
