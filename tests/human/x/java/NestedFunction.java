public class NestedFunction {
    public static void main(String[] args) {
        System.out.println(outer(3));
    }

    static int outer(int x) {
        class Inner {
            int inner(int y) { return x + y; }
        }
        return new Inner().inner(5);
    }
}
