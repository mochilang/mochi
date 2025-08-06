public class Main {

    static int and_gate(int input_1, int input_2) {
        if (input_1 != 0 && input_2 != 0) {
            return 1;
        }
        return 0;
    }

    static int n_input_and_gate(int[] inputs) {
        int i = 0;
        while (i < inputs.length) {
            if (inputs[i] == 0) {
                return 0;
            }
            i = i + 1;
        }
        return 1;
    }
    public static void main(String[] args) {
        System.out.println(and_gate(0, 0));
        System.out.println(and_gate(0, 1));
        System.out.println(and_gate(1, 0));
        System.out.println(and_gate(1, 1));
        System.out.println(n_input_and_gate(((int[])(new int[]{1, 0, 1, 1, 0}))));
        System.out.println(n_input_and_gate(((int[])(new int[]{1, 1, 1, 1, 1}))));
    }
}
