public class Main {
    static String dna = "" + "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG" + "CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG" + "AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT" + "GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT" + "CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG" + "TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA" + "TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT" + "CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG" + "TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC" + "GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT";
    static int le = dna.length();
    static int i = 0;
    static int a = 0;
    static int c = 0;
    static int g = 0;
    static int t = 0;
    static int idx = 0;

    static String padLeft(String s, int w) {
        String res = "";
        int n = w - s.length();
        while (n > 0) {
            res = res + " ";
            n = n - 1;
        }
        return res + s;
    }
    public static void main(String[] args) {
        System.out.println("SEQUENCE:");
        while (i < le) {
            int k = i + 50;
            if (k > le) {
                k = le;
            }
            System.out.println(String.valueOf(padLeft(String.valueOf(i), 5)) + ": " + dna.substring(i, k));
            i = i + 50;
        }
        while (idx < le) {
            String ch = dna.substring(idx, idx + 1);
            if ((ch.equals("A"))) {
                a = a + 1;
            } else             if ((ch.equals("C"))) {
                c = c + 1;
            } else             if ((ch.equals("G"))) {
                g = g + 1;
            } else             if ((ch.equals("T"))) {
                t = t + 1;
            }
            idx = idx + 1;
        }
        System.out.println("");
        System.out.println("BASE COUNT:");
        System.out.println("    A: " + String.valueOf(padLeft(String.valueOf(a), 3)));
        System.out.println("    C: " + String.valueOf(padLeft(String.valueOf(c), 3)));
        System.out.println("    G: " + String.valueOf(padLeft(String.valueOf(g), 3)));
        System.out.println("    T: " + String.valueOf(padLeft(String.valueOf(t), 3)));
        System.out.println("    ------");
        System.out.println("    Σ: " + String.valueOf(le));
        System.out.println("    ======");
    }
}
