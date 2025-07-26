public class Main {

    static int[] randInt(int s, int n) {
        int next = ((Number)(Math.floorMod((s * 1664525 + 1013904223), 2147483647))).intValue();
        return new int[]{next, Math.floorMod(next, n)};
    }

    static String padLeft(String s, int w) {
        String res = "";
        int n = w - s.length();
        while (n > 0) {
            res = res + " ";
            n = n - 1;
        }
        return res + s;
    }

    static Object[] makeSeq(int s, int le) {
        String bases = "ACGT";
        String out = "";
        int i = 0;
        while (i < le) {
            int[] r = randInt(s, 4);
            s = r[0];
            int idx = ((int)(r[1]));
            out = out + bases.substring(idx, idx + 1);
            i = i + 1;
        }
        return new Object[]{s, out};
    }

    static Object[] mutate(int s, String dna, int[] w) {
        String bases = "ACGT";
        int le = dna.length();
        int[] r = randInt(s, le);
        s = r[0];
        int p = ((int)(r[1]));
        r = randInt(s, 300);
        s = r[0];
        int x = ((int)(r[1]));
        String[] arr = new String[]{};
        int i = 0;
        while (i < le) {
            arr = java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(dna.substring(i, i + 1))).toArray(String[]::new);
            i = i + 1;
        }
        if (x < w[0]) {
            r = randInt(s, 4);
            s = r[0];
            int idx = ((int)(r[1]));
            String b = bases.substring(idx, idx + 1);
            System.out.println("  Change @" + String.valueOf(padLeft(String.valueOf(p), 3)) + " '" + arr[p] + "' to '" + b + "'");
arr[p] = b;
        } else         if (x < w[0] + w[1]) {
            System.out.println("  Delete @" + String.valueOf(padLeft(String.valueOf(p), 3)) + " '" + arr[p] + "'");
            int j = p;
            while (j < arr.length - 1) {
arr[j] = arr[j + 1];
                j = j + 1;
            }
            arr = java.util.Arrays.copyOfRange(arr, 0, arr.length - 1);
        } else {
            r = randInt(s, 4);
            s = r[0];
            int idx2 = ((int)(r[1]));
            String b = bases.substring(idx2, idx2 + 1);
            arr = java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of("")).toArray(String[]::new);
            int j = arr.length - 1;
            while (j > p) {
arr[j] = arr[j - 1];
                j = j - 1;
            }
            System.out.println("  Insert @" + String.valueOf(padLeft(String.valueOf(p), 3)) + " '" + b + "'");
arr[p] = b;
        }
        String out = "";
        i = 0;
        while (i < arr.length) {
            out = out + arr[i];
            i = i + 1;
        }
        return new Object[]{s, out};
    }

    static void prettyPrint(String dna, int rowLen) {
        System.out.println("SEQUENCE:");
        int le = dna.length();
        int i = 0;
        while (i < le) {
            int k = i + rowLen;
            if (k > le) {
                k = le;
            }
            System.out.println(String.valueOf(padLeft(String.valueOf(i), 5)) + ": " + dna.substring(i, k));
            i = i + rowLen;
        }
        int a = 0;
        int c = 0;
        int g = 0;
        int t = 0;
        int idx = 0;
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

    static String wstring(int[] w) {
        return "  Change: " + String.valueOf(w[0]) + "\n  Delete: " + String.valueOf(w[1]) + "\n  Insert: " + String.valueOf(w[2]) + "\n";
    }

    static void main() {
        int seed = 1;
        Object[] res = makeSeq(seed, 250);
        seed = ((Number)(res[0])).intValue();
        String dna = ((String)(res[1]));
        prettyPrint(dna, 50);
        int muts = 10;
        int[] w = new int[]{100, 100, 100};
        System.out.println("\nWEIGHTS (ex 300):");
        System.out.println(wstring(w));
        System.out.println("MUTATIONS (" + String.valueOf(muts) + "):");
        int i = 0;
        while (i < muts) {
            res = mutate(seed, dna, w);
            seed = ((Number)(res[0])).intValue();
            dna = ((String)(res[1]));
            i = i + 1;
        }
        System.out.println("");
        prettyPrint(dna, 50);
    }
    public static void main(String[] args) {
        main();
    }
}
