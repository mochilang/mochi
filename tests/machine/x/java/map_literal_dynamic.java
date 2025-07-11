class AB {
    int a;
    int b;
    AB(int a, int b) {
        this.a = a;
        this.b = b;
    }
    int size() { return 2; }
}
public class MapLiteralDynamic {
    public static void main(String[] args) {
    int x = 3;
    int y = 4;
    AB m = new AB(x, y);
    System.out.println(m.a + " " + m.b);
    }
}
