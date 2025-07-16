// bioinformatics-global-alignment.mochi
import java.util.*;

public class BioinformaticsGlobalAlignment {
    static String padLeft(String s, int w) {
        String res = "";
        int n = w - s.length();
        while (n > 0) {
            res = res + " ";
            n = (int)(n - 1);
        }
        return res + s;
    }
    static int indexOfFrom(String s, String ch, int start) {
        int i = start;
        while (i < s.length()) {
            if (Objects.equals(s.substring(i, i + 1), ch)) {
                return i;
            }
            i = (int)(i + 1);
        }
        return -1;
    }
    static boolean containsStr(String s, String sub) {
        int i = 0;
        int sl = s.length();
        int subl = sub.length();
        while (i <= sl - subl) {
            if (Objects.equals(s.substring(i, i + subl), sub)) {
                return true;
            }
            i = (int)(i + 1);
        }
        return false;
    }
    static List<String> distinct(List<String> slist) {
        List<String> res = Arrays.asList();
        for (String s : slist) {
            boolean found = false;
            for (String r : res) {
                if (Objects.equals(r, s)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                res.add(s);
            }
        }
        return res;
    }
    static List<List<String>> permutations(List<String> xs) {
        if (xs.size() <= 1) {
            return Arrays.asList(xs);
        }
        List<List<String>> res = Arrays.asList();
        int i = 0;
        while (i < xs.size()) {
            List<String> rest = Arrays.asList();
            int j = 0;
            while (j < xs.size()) {
                if (j != i) {
                    rest.add(xs.get(j));
                }
                j = (int)(j + 1);
            }
            List<List<String>> subs = permutations(rest);
            for (List<String> p : subs) {
                List<String> perm = Arrays.asList(xs.get(i));
                int k = 0;
                while (k < p.size()) {
                    perm.add(p.get(k));
                    k = (int)(k + 1);
                }
                res.add(perm);
            }
            i = (int)(i + 1);
        }
        return res;
    }
    static int headTailOverlap(String s1, String s2) {
        int start = 0;
        while (true) {
            int ix = indexOfFrom(s1, s2.substring(0, 1), start);
            if (Objects.equals(ix, 0 - 1)) {
                return 0;
            }
            start = (int)(ix);
            if (Objects.equals(s2.substring(0, s1.length() - start), s1.substring(start, s1.length()))) {
                return s1.length() - start;
            }
            start = (int)(start + 1);
        }
    }
    static List<String> deduplicate(List<String> slist) {
        List<String> arr = distinct(slist);
        List<String> filtered = Arrays.asList();
        int i = 0;
        while (i < arr.size()) {
            List<String> s1 = arr.get(i);
            boolean within = false;
            int j = 0;
            while (j < arr.size()) {
                if (j != i && containsStr(arr.get(j), s1)) {
                    within = true;
                    break;
                }
                j = (int)(j + 1);
            }
            if (!within) {
                filtered.add(s1);
            }
            i = (int)(i + 1);
        }
        return filtered;
    }
    static String joinAll(List<String> ss) {
        String out = "";
        for (String s : ss) {
            out = out + s;
        }
        return out;
    }
    static String shortestCommonSuperstring(List<String> slist) {
        List<String> ss = deduplicate(slist);
        String shortest = joinAll(ss);
        List<List<String>> perms = permutations(ss);
        int idx = 0;
        while (idx < perms.size()) {
            List<List<String>> perm = perms.get(idx);
            List<List<String>> sup = perm.get(0);
            int i = 0;
            while (i < ss.size() - 1) {
                int ov = headTailOverlap(perm.get(i), perm.get(i + 1));
                sup = sup + ((Number)perm.get(i + 1).substring(ov, perm.get(i + 1).size())).doubleValue();
                i = (int)(i + 1);
            }
            if (String.valueOf(sup.size()).compareTo(String.valueOf(shortest.length())) < 0) {
                shortest = sup;
            }
            idx = (int)(idx + 1);
        }
        return shortest;
    }
    static void printCounts(String seq) {
        int a = 0;
        int c = 0;
        int g = 0;
        int t = 0;
        int i = 0;
        while (i < seq.length()) {
            String ch = seq.substring(i, i + 1);
            if (Objects.equals(ch, "A")) {
                a = (int)(a + 1);
            }
            else {
                if (Objects.equals(ch, "C")) {
                    c = (int)(c + 1);
                }
                else {
                    if (Objects.equals(ch, "G")) {
                        g = (int)(g + 1);
                    }
                    else {
                        if (Objects.equals(ch, "T")) {
                            t = (int)(t + 1);
                        }
                    }
                }
            }
            i = (int)(i + 1);
        }
        int total = seq.length();
        System.out.println("\nNucleotide counts for " + seq + ":\n");
        System.out.println(padLeft("A", 10) + padLeft(String.valueOf(a), 12));
        System.out.println(padLeft("C", 10) + padLeft(String.valueOf(c), 12));
        System.out.println(padLeft("G", 10) + padLeft(String.valueOf(g), 12));
        System.out.println(padLeft("T", 10) + padLeft(String.valueOf(t), 12));
        System.out.println(padLeft("Other", 10) + padLeft(String.valueOf(total - (a + c + g + t)), 12));
        System.out.println("  ____________________");
        System.out.println(padLeft("Total length", 14) + padLeft(String.valueOf(total), 8));
    }
    static void main() {
        List<List<String>> tests = Arrays.asList(Arrays.asList("TA", "AAG", "TA", "GAA", "TA"), Arrays.asList("CATTAGGG", "ATTAG", "GGG", "TA"), Arrays.asList("AAGAUGGA", "GGAGCGCAUC", "AUCGCAAUAAGGA"), Arrays.asList("ATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTAT", "GGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGT", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "AACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT", "GCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTC", "CGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATTCTGCTTATAACACTATGTTCT", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATGCTCGTGC", "GATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATT", "TTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TCTCTTAAACTCCTGCTAAATGCTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGA"));
        for (List<String> seqs : tests) {
            String scs = shortestCommonSuperstring(seqs);
            printCounts(scs);
        }
    }
    static <T> List<T> append(List<T> list, T item) {
        List<T> res = new ArrayList<>(list);
        res.add(item);
        return res;
    }
    public static void main(String[] args) {
    main();
    }
}
