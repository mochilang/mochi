public class Main {

    static int not_gate(int input) {
        if (input == 0) {
            return 1;
        }
        return 0;
    }
    public static void main(String[] args) {
        System.out.println(not_gate(0));
        System.out.println(not_gate(1));
    }
}
