public class Main {
    static String alphabet;

    static boolean contains(String[] xs, String x) {
        int i = 0;
        while (i < xs.length) {
            if ((xs[i].equals(x))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static String[] remove_item(String[] xs, String x) {
        String[] res = ((String[])(new String[]{}));
        boolean removed = false;
        int i_1 = 0;
        while (i_1 < xs.length) {
            if (!removed && (xs[i_1].equals(x))) {
                removed = true;
            } else {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(xs[i_1])).toArray(String[]::new)));
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static String[] word_ladder(String current, String[] path, String target, String[] words) {
        if ((current.equals(target))) {
            return path;
        }
        int i_2 = 0;
        while (i_2 < _runeLen(current)) {
            int j = 0;
            while (j < _runeLen(alphabet)) {
                String c = _substr(alphabet, j, j + 1);
                String transformed = _substr(current, 0, i_2) + c + _substr(current, i_2 + 1, _runeLen(current));
                if (((Boolean)(contains(((String[])(words)), transformed)))) {
                    String[] new_words = ((String[])(remove_item(((String[])(words)), transformed)));
                    String[] new_path = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path), java.util.stream.Stream.of(transformed)).toArray(String[]::new)));
                    String[] result = ((String[])(word_ladder(transformed, ((String[])(new_path)), target, ((String[])(new_words)))));
                    if (result.length > 0) {
                        return result;
                    }
                }
                j = j + 1;
            }
            i_2 = i_2 + 1;
        }
        return new String[]{};
    }

    static void main() {
        String[] w1 = ((String[])(new String[]{"hot", "dot", "dog", "lot", "log", "cog"}));
        System.out.println(_p(word_ladder("hit", ((String[])(new String[]{"hit"})), "cog", ((String[])(w1)))));
        String[] w2 = ((String[])(new String[]{"hot", "dot", "dog", "lot", "log"}));
        System.out.println(_p(word_ladder("hit", ((String[])(new String[]{"hit"})), "cog", ((String[])(w2)))));
        String[] w3 = ((String[])(new String[]{"load", "goad", "gold", "lead", "lord"}));
        System.out.println(_p(word_ladder("lead", ((String[])(new String[]{"lead"})), "gold", ((String[])(w3)))));
        String[] w4 = ((String[])(new String[]{"came", "cage", "code", "cade", "gave"}));
        System.out.println(_p(word_ladder("game", ((String[])(new String[]{"game"})), "code", ((String[])(w4)))));
    }
    public static void main(String[] args) {
        alphabet = "abcdefghijklmnopqrstuvwxyz";
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
        return out;
    }

    static String _repeat(String s, int n) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++) sb.append(s);
        return sb.toString();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static void json(Object v) {
        System.out.println(_json(v));
    }

    static String _json(Object v) {
        if (v == null) return "null";
        if (v instanceof String) {
            String s = (String)v;
            s = s.replace("\\", "\\\\").replace("\"", "\\\"");
            return "\"" + s + "\"";
        }
        if (v instanceof Number || v instanceof Boolean) {
            return String.valueOf(v);
        }
        if (v instanceof int[]) {
            int[] a = (int[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof double[]) {
            double[] a = (double[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof boolean[]) {
            boolean[] a = (boolean[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v.getClass().isArray()) {
            Object[] a = (Object[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(_json(a[i])); }
            sb.append("]");
            return sb.toString();
        }
        String s = String.valueOf(v);
        s = s.replace("\\", "\\\\").replace("\"", "\\\"");
        return "\"" + s + "\"";
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
