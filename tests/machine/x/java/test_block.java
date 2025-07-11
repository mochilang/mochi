public class TestBlock {
    public static void main(String[] args) {
    int x = 1 + 2;
    if (!(x == 3)) throw new AssertionError("expect failed");
    System.out.println("ok");
    }
}
