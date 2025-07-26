public class Main {

    static int indexOfStr(String h, String n) {
        int hlen = h.length();
        int nlen = n.length();
        if (nlen == 0) {
            return 0;
        }
        int i = 0;
        while (i <= hlen - nlen) {
            if ((h.substring(i, i + nlen).equals(n))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int stringSearchSingle(String h, String n) {
        return indexOfStr(h, n);
    }

    static int[] stringSearch(String h, String n) {
        int[] result = new int[]{};
        int start = 0;
        int hlen = h.length();
        int nlen = n.length();
        while (start < hlen) {
            int idx = indexOfStr(h.substring(start, hlen), n);
            if (idx >= 0) {
                result = java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(start + idx)).toArray();
                start = start + idx + nlen;
            } else {
                break;
            }
        }
        return result;
    }

    static String display(int[] nums) {
        String s = "[";
        int i = 0;
        while (i < nums.length) {
            if (i > 0) {
                s = s + ", ";
            }
            s = s + String.valueOf(nums[i]);
            i = i + 1;
        }
        s = s + "]";
        return s;
    }

    static void main() {
        String[] texts = new String[]{"GCTAGCTCTACGAGTCTA", "GGCTATAATGCGTA", "there would have been a time for such a word", "needle need noodle needle", "DKnuthusesandprogramsanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguages", "Nearby farms grew an acre of alfalfa on the dairy's behalf, with bales of that alfalfa exchanged for milk."};
        String[] patterns = new String[]{"TCTA", "TAATAAA", "word", "needle", "and", "alfalfa"};
        int i = 0;
        while (i < texts.length) {
            System.out.println("text" + String.valueOf(i + 1) + " = " + String.valueOf(texts[i]));
            i = i + 1;
        }
        System.out.println("");
        int j = 0;
        while (j < texts.length) {
            int[] idxs = stringSearch(String.valueOf(texts[j]), String.valueOf(patterns[j]));
            System.out.println("Found \"" + String.valueOf(patterns[j]) + "\" in 'text" + String.valueOf(j + 1) + "' at indexes " + String.valueOf(display(idxs)));
            j = j + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }
}
