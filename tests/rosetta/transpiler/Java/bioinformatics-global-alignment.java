public class Main {

    static String padLeft(String s, int w) {
        String res = "";
        int n = w - s.length();
        while (n > 0) {
            res = res + " ";
            n = n - 1;
        }
        return res + s;
    }

    static int indexOfFrom(String s, String ch, int start) {
        int i = start;
        while (i < s.length()) {
            if ((s.substring(i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static boolean containsStr(String s, String sub) {
        int i = 0;
        int sl = s.length();
        int subl = sub.length();
        while (i <= sl - subl) {
            if ((s.substring(i, i + subl).equals(sub))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static String[] distinct(String[] slist) {
        String[] res = new String[]{};
        for (String s : slist) {
            boolean found = false;
            for (String r : res) {
                if ((r.equals(s))) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                res = java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(s)).toArray(String[]::new);
            }
        }
        return res;
    }

    static String[][] permutations(String[] xs) {
        if (xs.length <= 1) {
            return new String[][]{xs};
        }
        String[][] res = new String[][]{};
        int i = 0;
        while (i < xs.length) {
            String[] rest = new String[]{};
            int j = 0;
            while (j < xs.length) {
                if (j != i) {
                    rest = java.util.stream.Stream.concat(java.util.Arrays.stream(rest), java.util.stream.Stream.of(xs[j])).toArray(String[]::new);
                }
                j = j + 1;
            }
            String[][] subs = permutations(rest);
            for (String[] p : subs) {
                String[] perm = new String[]{xs[i]};
                int k = 0;
                while (k < p.length) {
                    perm = java.util.stream.Stream.concat(java.util.Arrays.stream(perm), java.util.stream.Stream.of(p[k])).toArray(String[]::new);
                    k = k + 1;
                }
                res = appendObj(res, perm);
            }
            i = i + 1;
        }
        return res;
    }

    static int headTailOverlap(String s1, String s2) {
        int start = 0;
        while (true) {
            int ix = indexOfFrom(s1, s2.substring(0, 1), start);
            if (ix == 0 - 1) {
                return 0;
            }
            start = ix;
            int sublen = s1.length() - start;
            if (sublen > s2.length()) {
                sublen = s2.length();
            }
            if ((s2.substring(0, sublen).equals(s1.substring(start, start + sublen)))) {
                return sublen;
            }
            start = start + 1;
        }
    }

    static String[] deduplicate(String[] slist) {
        String[] arr = distinct(slist);
        String[] filtered = new String[]{};
        int i = 0;
        while (i < arr.length) {
            String s1 = arr[i];
            boolean within = false;
            int j = 0;
            while (j < arr.length) {
                if (j != i && containsStr(arr[j], s1)) {
                    within = true;
                    break;
                }
                j = j + 1;
            }
            if (!within) {
                filtered = java.util.stream.Stream.concat(java.util.Arrays.stream(filtered), java.util.stream.Stream.of(s1)).toArray(String[]::new);
            }
            i = i + 1;
        }
        return filtered;
    }

    static String joinAll(String[] ss) {
        String out = "";
        for (String s : ss) {
            out = out + s;
        }
        return out;
    }

    static String shortestCommonSuperstring(String[] slist) {
        String[] ss = deduplicate(slist);
        String shortest = String.valueOf(joinAll(ss));
        String[][] perms = permutations(ss);
        int idx = 0;
        while (idx < perms.length) {
            String[] perm = perms[idx];
            String sup = perm[0];
            int i = 0;
            while (i < ss.length - 1) {
                int ov = headTailOverlap(perm[i], perm[i + 1]);
                sup = sup + perm[i + 1].substring(ov, perm[i + 1].length());
                i = i + 1;
            }
            if (sup.length() < shortest.length()) {
                shortest = sup;
            }
            idx = idx + 1;
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
            if ((ch.equals("A"))) {
                a = a + 1;
            } else             if ((ch.equals("C"))) {
                c = c + 1;
            } else             if ((ch.equals("G"))) {
                g = g + 1;
            } else             if ((ch.equals("T"))) {
                t = t + 1;
            }
            i = i + 1;
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
        String[][] tests = new String[][]{new String[]{"TA", "AAG", "TA", "GAA", "TA"}, new String[]{"CATTAGGG", "ATTAG", "GGG", "TA"}, new String[]{"AAGAUGGA", "GGAGCGCAUC", "AUCGCAAUAAGGA"}, new String[]{"ATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTAT", "GGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGT", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "AACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT", "GCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTC", "CGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATTCTGCTTATAACACTATGTTCT", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATGCTCGTGC", "GATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATT", "TTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TCTCTTAAACTCCTGCTAAATGCTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGA"}};
        for (String[] seqs : tests) {
            String scs = String.valueOf(shortestCommonSuperstring(seqs));
            printCounts(scs);
        }
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
