public class Main {
    static int[] a = new int[]{1, 2, 3};
    static int[] b = new int[]{7, 12, 60};
    static Object[] i = new Object[]{1, 2, 3};
    static Object[] j = new Object[]{"Crosby", "Stills", "Nash", "Young"};
    static int[] l = new int[]{1, 2, 3};
    static int[] m = new int[]{7, 12, 60};

    static int[] concatInts(int[] a, int[] b) {
        int[] out = new int[]{};
        for (int v : a) {
            out = java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(v)).toArray();
        }
        for (int v : b) {
            out = java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(v)).toArray();
        }
        return out;
    }

    static Object[] concatAny(Object[] a, Object[] b) {
        Object[] out = new Object[]{};
        for (Object v : a) {
            out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(v)).toArray(Object[]::new);
        }
        for (Object v : b) {
            out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(v)).toArray(Object[]::new);
        }
        return out;
    }
    public static void main(String[] args) {
        System.out.println(String.valueOf(concatInts(a, b)));
        System.out.println(String.valueOf(concatAny(i, j)));
        System.out.println(String.valueOf(concatInts(l, m)));
    }
}
