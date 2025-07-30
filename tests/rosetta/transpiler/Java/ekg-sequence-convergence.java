public class Main {

    static boolean contains(int[] xs, int n) {
        int i = 0;
        while (i < xs.length) {
            if (xs[i] == n) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static int gcd(int a, int b) {
        int x = a;
        int y = b;
        while (y != 0) {
            int t = Math.floorMod(x, y);
            x = y;
            y = t;
        }
        if (x < 0) {
            x = -x;
        }
        return x;
    }

    static int[] sortInts(int[] xs) {
        int[] arr = xs;
        int n = arr.length;
        int i = 0;
        while (i < n) {
            int j = 0;
            while (j < n - 1) {
                if (arr[j] > arr[j + 1]) {
                    int tmp = arr[j];
arr[j] = arr[j + 1];
arr[j + 1] = tmp;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return arr;
    }

    static boolean areSame(int[] s, int[] t) {
        if (s.length != t.length) {
            return false;
        }
        int[] a = sortInts(s);
        int[] b = sortInts(t);
        int i = 0;
        while (i < a.length) {
            if (a[i] != b[i]) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static void printSlice(int start, int[] seq) {
        int[] first = new int[]{};
        int i = 0;
        while (i < 30) {
            first = java.util.stream.IntStream.concat(java.util.Arrays.stream(first), java.util.stream.IntStream.of(seq[i])).toArray();
            i = i + 1;
        }
        String pad = "";
        if (start < 10) {
            pad = " ";
        }
        System.out.println("EKG(" + pad + String.valueOf(start) + "): " + String.valueOf(first));
    }

    static void main() {
        int limit = 100;
        int[] starts = new int[]{2, 5, 7, 9, 10};
        int[][] ekg = new int[][]{};
        int s = 0;
        while (s < starts.length) {
            int[] seq = new int[]{1, starts[s]};
            int n = 2;
            while (n < limit) {
                int i = 2;
                boolean done = false;
                while (!done) {
                    if (!(Boolean)contains(seq, i) && gcd(seq[n - 1], i) > 1) {
                        seq = java.util.stream.IntStream.concat(java.util.Arrays.stream(seq), java.util.stream.IntStream.of(i)).toArray();
                        done = true;
                    }
                    i = i + 1;
                }
                n = n + 1;
            }
            ekg = appendObj(ekg, seq);
            printSlice(starts[s], seq);
            s = s + 1;
        }
        int i = 2;
        boolean found = false;
        while (i < limit) {
            if (ekg[1][i] == ekg[2][i] && areSame(java.util.Arrays.copyOfRange(ekg[1], 0, i), java.util.Arrays.copyOfRange(ekg[2], 0, i))) {
                System.out.println("\nEKG(5) and EKG(7) converge at term " + String.valueOf(i + 1));
                found = true;
                break;
            }
            i = i + 1;
        }
        if (!found) {
            System.out.println("\nEKG5(5) and EKG(7) do not converge within " + String.valueOf(limit) + " terms");
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
