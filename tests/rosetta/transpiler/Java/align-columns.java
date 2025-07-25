public class Main {
    static String text = String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf("Given$a$text$file$of$many$lines,$where$fields$within$a$line\n" + "are$delineated$by$a$single$'dollar'$character,$write$a$program\n") + "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each\n") + "column$are$separated$by$at$least$one$space.\n") + "Further,$allow$for$each$word$in$a$column$to$be$either$left\n") + "justified,$right$justified,$or$center$justified$within$its$column.");
    static java.util.Map<String,Object> f = newFormatter(text);

    static String[] split(String s, String sep) {
        String[] parts = new String[]{};
        String cur = "";
        int i = 0;
        while (i < s.length()) {
            if ((sep.length() > 0 && i + sep.length() <= s.length() && s.substring(i, i + sep.length()).equals(sep))) {
                parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                cur = "";
                i = i + sep.length();
            } else {
                cur = String.valueOf(cur + s.substring(i, i + 1));
                i = i + 1;
            }
        }
        parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        return parts;
    }

    static String[] rstripEmpty(String[] words) {
        int n = words.length;
        while ((n > 0 && words[n - 1].equals(""))) {
            n = n - 1;
        }
        return java.util.Arrays.copyOfRange(words, 0, n);
    }

    static String spaces(int n) {
        String out = "";
        int i = 0;
        while (i < n) {
            out = String.valueOf(out + " ");
            i = i + 1;
        }
        return out;
    }

    static String pad(String word, int width, int align) {
        int diff = width - word.length();
        if (align == 0) {
            return word + String.valueOf(spaces(diff));
        }
        if (align == 2) {
            return String.valueOf(spaces(diff)) + word;
        }
        int left = ((Number)((diff / 2))).intValue();
        int right = diff - left;
        return String.valueOf(spaces(left)) + word + spaces(right);
    }

    static java.util.Map<String,Object> newFormatter(String text) {
        String[] lines = split(text, "\n");
        String[][] fmtLines = new String[][]{};
        int[] width = new int[]{};
        int i = 0;
        while (i < lines.length) {
            if (lines[i].length() == 0) {
                i = i + 1;
                continue;
            }
            String[] words = rstripEmpty(split(String.valueOf(lines[i]), "$"));
            fmtLines = appendObj(fmtLines, words);
            int j = 0;
            while (j < words.length) {
                int wlen = words[j].length();
                if (j == width.length) {
                    width = java.util.stream.IntStream.concat(java.util.Arrays.stream(width), java.util.stream.IntStream.of(wlen)).toArray();
                } else                 if (wlen > width[j]) {
width[j] = wlen;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("text", fmtLines), java.util.Map.entry("width", width)));
    }

    static void printFmt(java.util.Map<String,Object> f, int align) {
        String[][] lines = (String[][])(((String[][])f.get("text")));
        int[] width = (int[])(((int[])f.get("width")));
        int i = 0;
        while (i < lines.length) {
            String[] words = lines[i];
            String line = "";
            int j = 0;
            while (j < words.length) {
                line = String.valueOf(String.valueOf(line + String.valueOf(pad(String.valueOf(words[j]), width[j], align))) + " ");
                j = j + 1;
            }
            System.out.println(line);
            i = i + 1;
        }
        System.out.println("");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            printFmt(f, 0);
            printFmt(f, 1);
            printFmt(f, 2);
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
        return rt.totalMemory() - rt.freeMemory();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
