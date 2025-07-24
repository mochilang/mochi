public class Main {

    static String sortRunes(String s) {
        String[] arr = new String[]{};
        int i = 0;
        while (i < s.length()) {
            arr = java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(s.substring(i, i + 1))).toArray(String[]::new);
            i = i + 1;
        }
        int n = arr.length;
        int m = 0;
        while (m < n) {
            int j = 0;
            while (j < n - 1) {
                if (arr[j] > arr[j + 1]) {
                    String tmp = arr[j];
arr[j] = arr[j + 1];
arr[j + 1] = tmp;
                }
                j = j + 1;
            }
            m = m + 1;
        }
        String out = "";
        i = 0;
        while (i < n) {
            out = out + arr[i];
            i = i + 1;
        }
        return out;
    }

    static String[] sortStrings(String[] xs) {
        String[] res = new String[]{};
        String[] tmp = xs;
        while (tmp.length > 0) {
            String min = tmp[0];
            int idx = 0;
            int i = 1;
            while (i < tmp.length) {
                if ((tmp[i].compareTo(min) < 0)) {
                    min = tmp[i];
                    idx = i;
                }
                i = i + 1;
            }
            res = java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(min)).toArray(String[]::new);
            String[] out = new String[]{};
            int j = 0;
            while (j < tmp.length) {
                if (j != idx) {
                    out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(tmp[j])).toArray(String[]::new);
                }
                j = j + 1;
            }
            tmp = out;
        }
        return res;
    }

    static void main() {
        String[] words = new String[]{"abel", "able", "bale", "bela", "elba", "alger", "glare", "lager", "large", "regal", "angel", "angle", "galen", "glean", "lange", "caret", "carte", "cater", "crate", "trace", "elan", "lane", "lean", "lena", "neal", "evil", "levi", "live", "veil", "vile"};
        java.util.Map<String,Object> groups = new java.util.LinkedHashMap<String, Object>();
        int maxLen = 0;
        for (var w : words) {
            String k = sortRunes(w);
            if (!(Boolean)(groups.containsKey(k))) {
groups.put(k, new int[]{w});
            } else {
groups.put(k, java.util.stream.Stream.concat(java.util.Arrays.stream((Object)(groups.get(k))), java.util.stream.Stream.of(w)).toArray(String[]::new));
            }
            if ((Object)(groups.get(k)).length() > maxLen) {
                maxLen = (Object)(groups.get(k)).length();
            }
        }
        java.util.Map<String,Boolean> printed = new java.util.LinkedHashMap<String, Object>();
        for (var w : words) {
            String k = sortRunes(w);
            if ((Object)(groups.get(k)).length() == maxLen) {
                if (!(Boolean)(printed.containsKey(k))) {
                    String[] g = sortStrings((Object)(groups.get(k)));
                    String line = "[" + g[0];
                    int i = 1;
                    while (i < g.length) {
                        line = line + " " + g[i];
                        i = i + 1;
                    }
                    line = line + "]";
                    System.out.println(line);
printed.put(k, true);
                }
            }
        }
    }
    public static void main(String[] args) {
        main();
    }
}
