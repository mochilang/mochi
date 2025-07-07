public class TestBlock {
    public static void main(String[] args) {
        int x = 1 + 2;
        if (x != 3) {
            throw new AssertionError("expected 3");
        }
        System.out.println("ok");
    }
}
