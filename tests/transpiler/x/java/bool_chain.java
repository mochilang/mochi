public class Main {

    static boolean boom() {
        System.out.println("boom");
        return true;
    }
    public static void main(String[] args) {
        System.out.println((1 < 2) && (2 < 3) && (3 < 4) ? 1 : 0);
        System.out.println((1 < 2) && (2 > 3) && boom() ? 1 : 0);
        System.out.println((1 < 2) && (2 < 3) && (3 > 4) && boom() ? 1 : 0);
    }
}
