public class Main {

    static int or_gate(int input_1, int input_2) {
        if (((Boolean)(java.util.Arrays.stream(new int[]{input_1, input_2}).anyMatch((x) -> ((Number)(x)).intValue() == 1)))) {
            return 1;
        }
        return 0;
    }
    public static void main(String[] args) {
        System.out.println(or_gate(0, 0));
        System.out.println(or_gate(0, 1));
        System.out.println(or_gate(1, 0));
        System.out.println(or_gate(1, 1));
    }
}
