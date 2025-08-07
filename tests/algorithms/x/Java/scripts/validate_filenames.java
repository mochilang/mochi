public class Main {

    static int indexOf(String s, String sub) {
        int n = _runeLen(s);
        int m = _runeLen(sub);
        int i = 0;
        while (i <= n - m) {
            if ((_substr(s, i, i + m).equals(sub))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static boolean contains(String s, String sub) {
        return indexOf(s, sub) >= 0;
    }

    static int validate(String[] files) {
        String[] upper = ((String[])(new String[]{}));
        String[] space = ((String[])(new String[]{}));
        String[] hyphen = ((String[])(new String[]{}));
        String[] nodir = ((String[])(new String[]{}));
        for (String f : files) {
            if (!(f.equals(f.toLowerCase()))) {
                upper = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(upper), java.util.stream.Stream.of(f)).toArray(String[]::new)));
            }
            if (((Boolean)(contains(f, " ")))) {
                space = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(space), java.util.stream.Stream.of(f)).toArray(String[]::new)));
            }
            if (((Boolean)(contains(f, "-"))) && contains(f, "/site-packages/") == false) {
                hyphen = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(hyphen), java.util.stream.Stream.of(f)).toArray(String[]::new)));
            }
            if (!(Boolean)contains(f, "/")) {
                nodir = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodir), java.util.stream.Stream.of(f)).toArray(String[]::new)));
            }
        }
        if (upper.length > 0) {
            System.out.println(_p(upper.length) + " files contain uppercase characters:");
            for (String f : upper) {
                System.out.println(f);
            }
            System.out.println("");
        }
        if (space.length > 0) {
            System.out.println(_p(space.length) + " files contain space characters:");
            for (String f : space) {
                System.out.println(f);
            }
            System.out.println("");
        }
        if (hyphen.length > 0) {
            System.out.println(_p(hyphen.length) + " files contain hyphen characters:");
            for (String f : hyphen) {
                System.out.println(f);
            }
            System.out.println("");
        }
        if (nodir.length > 0) {
            System.out.println(_p(nodir.length) + " files are not in a directory:");
            for (String f : nodir) {
                System.out.println(f);
            }
            System.out.println("");
        }
        return upper.length + space.length + hyphen.length + nodir.length;
    }

    static void main() {
        String[] files = ((String[])(new String[]{"scripts/Validate_filenames.py", "good/file.txt", "bad file.txt", "/site-packages/pkg-name.py", "nopath", "src/hyphen-name.py"}));
        int bad = validate(((String[])(files)));
        System.out.println(_p(bad));
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
