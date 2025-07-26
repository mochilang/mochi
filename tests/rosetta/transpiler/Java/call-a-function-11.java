public class Main {

    static int zeroval(int ival) {
        int x = ival;
        x = 0;
        return x;
    }

    static void zeroptr(int[] ref) {
ref[0] = 0;
    }

    static void main() {
        int i = 1;
        System.out.println("initial: " + String.valueOf(i));
        int tmp = zeroval(i);
        System.out.println("zeroval: " + String.valueOf(i));
        int[] box = new int[]{i};
        zeroptr(box);
        i = box[0];
        System.out.println("zeroptr: " + String.valueOf(i));
        System.out.println("pointer: 0");
    }
    public static void main(String[] args) {
        main();
    }
}
