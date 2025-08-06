public class Main {

    static int mux(int input0, int input1, int select) {
        if ((input0 != 0 && input0 != 1) || (input1 != 0 && input1 != 1) || (select != 0 && select != 1)) {
            throw new RuntimeException(String.valueOf("Inputs and select signal must be 0 or 1"));
        }
        if (select == 1) {
            return input1;
        }
        return input0;
    }
    public static void main(String[] args) {
        System.out.println(mux(0, 1, 0));
        System.out.println(mux(0, 1, 1));
        System.out.println(mux(1, 0, 0));
        System.out.println(mux(1, 0, 1));
    }
}
