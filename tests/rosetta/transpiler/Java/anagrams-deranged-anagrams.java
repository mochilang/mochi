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

    static boolean deranged(String a, String b) {
        if (a.length() != b.length()) {
            return false;
        }
        int i = 0;
        while (i < a.length()) {
            if ((a.substring(i, i + 1).equals(b.substring(i, i + 1)))) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static void main() {
        String[] words = new String[]{"constitutionalism", "misconstitutional"};
        java.util.Map<String,Object> m = new java.util.LinkedHashMap<String, Object>();
        int bestLen = 0;
        String w1 = "";
        String w2 = "";
        for (var w : words) {
            if (w.length() <= bestLen) {
                continue;
            }
            String k = sortRunes(w);
            if (!(Boolean)(m.containsKey(k))) {
m.put(k, new int[]{w});
                continue;
            }
            for (var c : (Object)(m.get(k))) {
                if (deranged(w, c)) {
                    bestLen = w.length();
                    w1 = c;
                    w2 = w;
                    break;
                }
            }
m.put(k, java.util.stream.Stream.concat(java.util.Arrays.stream((Object)(m.get(k))), java.util.stream.Stream.of(w)).toArray(String[]::new));
        }
        System.out.println(w1 + " " + w2 + " : Length " + String.valueOf(bestLen));
    }
    public static void main(String[] args) {
        main();
    }
}
