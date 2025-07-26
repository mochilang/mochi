public class Main {
    static int[] b = new int[]{98, 105, 110, 97, 114, 121};
    static int[] c = b;
    static int[] d = new int[]{};
    static int i = 0;
    static int[] z = java.util.stream.IntStream.concat(java.util.Arrays.stream(b), java.util.stream.IntStream.of(122)).toArray();
    static int[] sub = java.util.Arrays.copyOfRange(b, 1, 3);
    static int[] f = new int[]{};
    static int[] rem = new int[]{};

    static String char_(int n) {
        String letters = "abcdefghijklmnopqrstuvwxyz";
        int idx = n - 97;
        if (idx < 0 || idx >= letters.length()) {
            return "?";
        }
        return letters.substring(idx, idx + 1);
    }

    static String fromBytes(int[] bs) {
        String s = "";
        int i = 0;
        while (i < bs.length) {
            s = s + String.valueOf(char_(bs[i]));
            i = i + 1;
        }
        return s;
    }
    public static void main(String[] args) {
        System.out.println(String.valueOf(b));
        System.out.println(String.valueOf(c));
        System.out.println(String.valueOf(b == c));
        while (i < b.length) {
            d = java.util.stream.IntStream.concat(java.util.Arrays.stream(d), java.util.stream.IntStream.of(b[i])).toArray();
            i = i + 1;
        }
d[1] = 97;
d[4] = 110;
        System.out.println(fromBytes(b));
        System.out.println(fromBytes(d));
        System.out.println(String.valueOf(b.length == 0));
        System.out.println(fromBytes(z));
        System.out.println(fromBytes(sub));
        i = 0;
        while (i < d.length) {
            int val = d[i];
            if (val == 110) {
                f = java.util.stream.IntStream.concat(java.util.Arrays.stream(f), java.util.stream.IntStream.of(109)).toArray();
            } else {
                f = java.util.stream.IntStream.concat(java.util.Arrays.stream(f), java.util.stream.IntStream.of(val)).toArray();
            }
            i = i + 1;
        }
        System.out.println(String.valueOf(fromBytes(d)) + " -> " + String.valueOf(fromBytes(f)));
        rem = java.util.stream.IntStream.concat(java.util.Arrays.stream(rem), java.util.stream.IntStream.of(b[0])).toArray();
        i = 3;
        while (i < b.length) {
            rem = java.util.stream.IntStream.concat(java.util.Arrays.stream(rem), java.util.stream.IntStream.of(b[i])).toArray();
            i = i + 1;
        }
        System.out.println(fromBytes(rem));
    }
}
