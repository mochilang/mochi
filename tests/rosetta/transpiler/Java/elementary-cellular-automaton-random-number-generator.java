public class Main {
    static int n;
    static int[] init;
    static int i;
    static int[] bytes;

    static int pow2(int k) {
        int v = 1;
        int i = 0;
        while (i < k) {
            v = v * 2;
            i = i + 1;
        }
        return v;
    }

    static int ruleBit(int ruleNum, int idx) {
        int r = ruleNum;
        int i = 0;
        while (i < idx) {
            r = r / 2;
            i = i + 1;
        }
        return Math.floorMod(r, 2);
    }

    static int[] evolve(int[] state, int ruleNum) {
        int[] out = new int[]{};
        int p = 0;
        while (p < 10) {
            int b = 0;
            int q = 7;
            while (q >= 0) {
                int[] st = state;
                b = b + st[0] * pow2(q);
                int[] next = new int[]{};
                int i = 0;
                while (i < n) {
                    int lidx = i - 1;
                    if (lidx < 0) {
                        lidx = n - 1;
                    }
                    int left = st[lidx];
                    int center = st[i];
                    int ridx = i + 1;
                    if (ridx >= n) {
                        ridx = 0;
                    }
                    int right = st[ridx];
                    int index = left * 4 + center * 2 + right;
                    next = java.util.stream.IntStream.concat(java.util.Arrays.stream(next), java.util.stream.IntStream.of(ruleBit(ruleNum, index))).toArray();
                    i = i + 1;
                }
                state = next;
                q = q - 1;
            }
            out = java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(b)).toArray();
            p = p + 1;
        }
        return out;
    }
    public static void main(String[] args) {
        n = 64;
        init = new int[]{};
        i = 0;
        while (i < n) {
            init = java.util.stream.IntStream.concat(java.util.Arrays.stream(init), java.util.stream.IntStream.of(0)).toArray();
            i = i + 1;
        }
init[0] = 1;
        bytes = evolve(init, 30);
        System.out.println(String.valueOf(bytes));
    }
}
