public class Main {

    static String trimSpace(String s) {
        int start = 0;
        while (start < _runeLen(s) && (s.substring(start, start + 1).equals(" "))) {
            start = start + 1;
        }
        int end = _runeLen(s);
        while (end > start && (s.substring(end - 1, end).equals(" "))) {
            end = end - 1;
        }
        return s.substring(start, end);
    }

    static boolean isUpper(String ch) {
        return (ch.compareTo("A") >= 0) && (ch.compareTo("Z") <= 0);
    }

    static String padLeft(String s, int w) {
        String res = "";
        int n = w - _runeLen(s);
        while (n > 0) {
            res = res + " ";
            n = n - 1;
        }
        return res + s;
    }

    static String snakeToCamel(String s) {
        s = String.valueOf(trimSpace(s));
        String out = "";
        boolean up = false;
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = s.substring(i, i + 1);
            if ((ch.equals("_")) || (ch.equals("-")) || (ch.equals(" ")) || (ch.equals("."))) {
                up = true;
                i = i + 1;
                continue;
            }
            if (i == 0) {
                out = out + ch.toLowerCase();
                up = false;
                i = i + 1;
                continue;
            }
            if (up) {
                out = out + ch.toUpperCase();
                up = false;
            } else {
                out = out + ch;
            }
            i = i + 1;
        }
        return out;
    }

    static String camelToSnake(String s) {
        s = String.valueOf(trimSpace(s));
        String out_1 = "";
        boolean prevUnd = false;
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String ch_1 = s.substring(i_1, i_1 + 1);
            if ((ch_1.equals(" ")) || (ch_1.equals("-")) || (ch_1.equals("."))) {
                if (!prevUnd && _runeLen(out_1) > 0) {
                    out_1 = out_1 + "_";
                    prevUnd = true;
                }
                i_1 = i_1 + 1;
                continue;
            }
            if ((ch_1.equals("_"))) {
                if (!prevUnd && _runeLen(out_1) > 0) {
                    out_1 = out_1 + "_";
                    prevUnd = true;
                }
                i_1 = i_1 + 1;
                continue;
            }
            if (((Boolean)(isUpper(ch_1)))) {
                if (i_1 > 0 && (!prevUnd)) {
                    out_1 = out_1 + "_";
                }
                out_1 = out_1 + ch_1.toLowerCase();
                prevUnd = false;
            } else {
                out_1 = out_1 + ch_1.toLowerCase();
                prevUnd = false;
            }
            i_1 = i_1 + 1;
        }
        int start_1 = 0;
        while (start_1 < _runeLen(out_1) && (out_1.substring(start_1, start_1 + 1).equals("_"))) {
            start_1 = start_1 + 1;
        }
        int end_1 = _runeLen(out_1);
        while (end_1 > start_1 && (out_1.substring(end_1 - 1, end_1).equals("_"))) {
            end_1 = end_1 - 1;
        }
        out_1 = out_1.substring(start_1, end_1);
        String res_1 = "";
        int j = 0;
        boolean lastUnd = false;
        while (j < _runeLen(out_1)) {
            String c = out_1.substring(j, j + 1);
            if ((c.equals("_"))) {
                if (!lastUnd) {
                    res_1 = res_1 + c;
                }
                lastUnd = true;
            } else {
                res_1 = res_1 + c;
                lastUnd = false;
            }
            j = j + 1;
        }
        return res_1;
    }

    static void main() {
        String[] samples = new String[]{"snakeCase", "snake_case", "snake-case", "snake case", "snake CASE", "snake.case", "variable_10_case", "variable10Case", "ɛrgo rE tHis", "hurry-up-joe!", "c://my-docs/happy_Flag-Day/12.doc", " spaces "};
        System.out.println("=== To snake_case ===");
        for (String s : samples) {
            System.out.println(String.valueOf(padLeft(s, 34)) + " => " + String.valueOf(camelToSnake(s)));
        }
        System.out.println("");
        System.out.println("=== To camelCase ===");
        for (String s : samples) {
            System.out.println(String.valueOf(padLeft(s, 34)) + " => " + String.valueOf(snakeToCamel(s)));
        }
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
