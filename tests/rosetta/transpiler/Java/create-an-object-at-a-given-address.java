public class Main {

    static String listStr(int[] xs) {
        String s = "[";
        int i = 0;
        while (i < xs.length) {
            s = s + String.valueOf(xs[i]);
            if (i < xs.length - 1) {
                s = s + " ";
            }
            i = i + 1;
        }
        s = s + "]";
        return s;
    }

    static void pointerDemo() {
        System.out.println("Pointer:");
        int i = 0;
        System.out.println("Before:");
        System.out.println("\t<address>: " + String.valueOf(i) + ", " + String.valueOf(i));
        i = 3;
        System.out.println("After:");
        System.out.println("\t<address>: " + String.valueOf(i) + ", " + String.valueOf(i));
    }

    static void sliceDemo() {
        System.out.println("Slice:");
        int[] a = new int[]{};
        for (int _v = 0; _v < 10; _v++) {
            a = java.util.stream.IntStream.concat(java.util.Arrays.stream(a), java.util.stream.IntStream.of(0)).toArray();
        }
        int[] s = a;
        System.out.println("Before:");
        System.out.println("\ts: " + String.valueOf(listStr(s)));
        System.out.println("\ta: " + String.valueOf(listStr(a)));
        int[] data = new int[]{65, 32, 115, 116, 114, 105, 110, 103, 46};
        int idx = 0;
        while (idx < data.length) {
s[idx] = data[idx];
            idx = idx + 1;
        }
        System.out.println("After:");
        System.out.println("\ts: " + String.valueOf(listStr(s)));
        System.out.println("\ta: " + String.valueOf(listStr(a)));
    }
    public static void main(String[] args) {
        pointerDemo();
        System.out.println("");
        sliceDemo();
    }
}
