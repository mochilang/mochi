public class IfThenElseNested {
    public static void main(String[] args) {
    int x = 8;
    Object msg = (x > 10 ? "big" : (x > 5 ? "medium" : "small"));
    System.out.println(msg);
    }
}
