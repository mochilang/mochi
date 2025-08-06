public class Main {

    static int xnor_gate(int a, int b) {
        if (a == b) {
            return 1;
        }
        return 0;
    }
    public static void main(String[] args) {
        System.out.println(xnor_gate(0, 0));
        System.out.println(xnor_gate(0, 1));
        System.out.println(xnor_gate(1, 0));
        System.out.println(xnor_gate(1, 1));
    }
}
