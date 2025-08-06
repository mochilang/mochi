public class Main {

    static int nimply_gate(int a, int b) {
        if (a == 1 && b == 0) {
            return 1;
        }
        return 0;
    }
    public static void main(String[] args) {
        System.out.println(nimply_gate(0, 0));
        System.out.println(nimply_gate(0, 1));
        System.out.println(nimply_gate(1, 0));
        System.out.println(nimply_gate(1, 1));
    }
}
