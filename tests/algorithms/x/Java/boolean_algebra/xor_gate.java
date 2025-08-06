public class Main {

    static int xor_gate(int input_1, int input_2) {
        int zeros = 0;
        if (input_1 == 0) {
            zeros = zeros + 1;
        }
        if (input_2 == 0) {
            zeros = zeros + 1;
        }
        return Math.floorMod(zeros, 2);
    }
    public static void main(String[] args) {
        System.out.println(xor_gate(0, 0));
        System.out.println(xor_gate(0, 1));
        System.out.println(xor_gate(1, 0));
        System.out.println(xor_gate(1, 1));
    }
}
