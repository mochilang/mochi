public class BoolChain {
    static boolean boom() {
        System.out.println("boom");
        return true;
    }

    public static void main(String[] args) {
        System.out.println((1 < 2) && (2 < 3) && (3 < 4));
        System.out.println((1 < 2) && (2 > 3) && boom());
        System.out.println((1 < 2) && (2 < 3) && (3 > 4) && boom());
    }
}
