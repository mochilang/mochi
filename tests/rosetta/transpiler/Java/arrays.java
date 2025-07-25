public class Main {
    static int[] a = new int[]{0, 0, 0, 0, 0};
    static int[] s = java.util.Arrays.copyOfRange(a, 0, 4);
    static int cap_s = 5;

    static String listStr(int[] xs) {
        String s = "[";
        int i = 0;
        while (i < xs.length) {
            s = String.valueOf(s + String.valueOf(xs[i]));
            if (i + 1 < xs.length) {
                s = String.valueOf(s + " ");
            }
            i = i + 1;
        }
        s = String.valueOf(s + "]");
        return s;
    }
    public static void main(String[] args) {
        System.out.println("len(a) = " + String.valueOf(a.length));
        System.out.println("a = " + String.valueOf(listStr(a)));
a[0] = 3;
        System.out.println("a = " + String.valueOf(listStr(a)));
        System.out.println("a[0] = " + String.valueOf(a[0]));
        System.out.println("s = " + String.valueOf(listStr(s)));
        System.out.println(String.valueOf(String.valueOf("len(s) = " + String.valueOf(s.length)) + "  cap(s) = ") + String.valueOf(cap_s));
        s = java.util.Arrays.copyOfRange(a, 0, 5);
        System.out.println("s = " + String.valueOf(listStr(s)));
a[0] = 22;
s[0] = 22;
        System.out.println("a = " + String.valueOf(listStr(a)));
        System.out.println("s = " + String.valueOf(listStr(s)));
        s = java.util.stream.IntStream.concat(java.util.Arrays.stream(s), java.util.stream.IntStream.of(4)).toArray();
        s = java.util.stream.IntStream.concat(java.util.Arrays.stream(s), java.util.stream.IntStream.of(5)).toArray();
        s = java.util.stream.IntStream.concat(java.util.Arrays.stream(s), java.util.stream.IntStream.of(6)).toArray();
        cap_s = 10;
        System.out.println("s = " + String.valueOf(listStr(s)));
        System.out.println(String.valueOf(String.valueOf("len(s) = " + String.valueOf(s.length)) + "  cap(s) = ") + String.valueOf(cap_s));
a[4] = -1;
        System.out.println("a = " + String.valueOf(listStr(a)));
        System.out.println("s = " + String.valueOf(listStr(s)));
        s = new int[]{};
        for (int i = 0; i < 8; i++) {
            s = java.util.stream.IntStream.concat(java.util.Arrays.stream(s), java.util.stream.IntStream.of(0)).toArray();
        }
        cap_s = 8;
        System.out.println("s = " + String.valueOf(listStr(s)));
        System.out.println(String.valueOf(String.valueOf("len(s) = " + String.valueOf(s.length)) + "  cap(s) = ") + String.valueOf(cap_s));
    }
}
