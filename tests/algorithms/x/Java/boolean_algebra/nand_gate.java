public class Main {

    static int nand_gate(int a, int b) {
        if (a != 0 && b != 0) {
            return 0;
        }
        return 1;
    }
    public static void main(String[] args) {
        System.out.println(nand_gate(0, 0));
        System.out.println(nand_gate(0, 1));
        System.out.println(nand_gate(1, 0));
        System.out.println(nand_gate(1, 1));
    }
}
