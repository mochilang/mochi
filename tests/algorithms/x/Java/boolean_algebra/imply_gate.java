public class Main {

    static int imply_gate(int input_1, int input_2) {
        if (input_1 == 0 || input_2 == 1) {
            return 1;
        }
        return 0;
    }
    public static void main(String[] args) {
        System.out.println(imply_gate(0, 0));
        System.out.println(imply_gate(0, 1));
        System.out.println(imply_gate(1, 0));
        System.out.println(imply_gate(1, 1));
    }
}
