public class Main {

    static boolean amb(string[][] wordsets, String[] res, int idx) {
        if (idx == wordsets.length) {
            return true;
        }
        String prev = "";
        if (idx > 0) {
            prev = res[idx - 1];
        }
        int i = 0;
        while (i < wordsets[idx].length()) {
            String w = wordsets[idx][i];
            if ((idx == 0 || prev.substring(prev.length() - 1, prev.length()).equals(w.substring(0, 1)))) {
res[idx] = w;
                if (amb(wordsets, res, idx + 1)) {
                    return true;
                }
            }
            i = i + 1;
        }
        return false;
    }

    static void main() {
        String[][] wordset = new String[][]{new String[]{"the", "that", "a"}, new String[]{"frog", "elephant", "thing"}, new String[]{"walked", "treaded", "grows"}, new String[]{"slowly", "quickly"}};
        String[] res = new String[]{};
        int i = 0;
        while (i < wordset.length) {
            res = java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of("")).toArray(String[]::new);
            i = i + 1;
        }
        if (amb(wordset, res, 0)) {
            String out = "[" + res[0];
            int j = 1;
            while (j < res.length) {
                out = out + " " + res[j];
                j = j + 1;
            }
            out = out + "]";
            System.out.println(out);
        } else {
            System.out.println("No amb found");
        }
    }
    public static void main(String[] args) {
        main();
    }
}
