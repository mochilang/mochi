public class Main {
    static int jobs;
    static class HailResult {
        int seq;
        int cnt;
        String out;
        HailResult(int seq, int cnt, String out) {
            this.seq = seq;
            this.cnt = cnt;
            this.out = out;
        }
        @Override public String toString() {
            return String.format("{'seq': %s, 'cnt': %s, 'out': '%s'}", String.valueOf(seq), String.valueOf(cnt), String.valueOf(out));
        }
    }


    static String pad(int n) {
        String s = String.valueOf(n);
        while (_runeLen(s) < 4) {
            s = " " + s;
        }
        return s;
    }

    static HailResult hail(int seq, int cnt) {
        String out = String.valueOf(pad(seq));
        if (seq != 1) {
            cnt = cnt + 1;
            if (Math.floorMod(seq, 2) != 0) {
                seq = 3 * seq + 1;
            } else {
                seq = seq / 2;
            }
        }
        return new HailResult(seq, cnt, out);
    }

    static void main() {
        int[] seqs = new int[]{};
        int[] cnts = new int[]{};
        for (int i = 0; i < jobs; i++) {
            seqs = java.util.stream.IntStream.concat(java.util.Arrays.stream(seqs), java.util.stream.IntStream.of(i + 1)).toArray();
            cnts = java.util.stream.IntStream.concat(java.util.Arrays.stream(cnts), java.util.stream.IntStream.of(0)).toArray();
        }
        while (true) {
            String line = "";
            int i = 0;
            while (i < jobs) {
                HailResult res = hail(seqs[i], cnts[i]);
seqs[i] = res.seq;
cnts[i] = res.cnt;
                line = line + res.out;
                i = i + 1;
            }
            System.out.println(line);
            boolean done = true;
            int j = 0;
            while (j < jobs) {
                if (seqs[j] != 1) {
                    done = false;
                }
                j = j + 1;
            }
            if (done) {
                break;
            }
        }
        System.out.println("");
        System.out.println("COUNTS:");
        String counts = "";
        int k = 0;
        while (k < jobs) {
            counts = counts + String.valueOf(pad(cnts[k]));
            k = k + 1;
        }
        System.out.println(counts);
        System.out.println("");
    }
    public static void main(String[] args) {
        jobs = 12;
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
