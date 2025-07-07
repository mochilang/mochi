public class IfThenElseNested {
    public static void main(String[] args) {
        int x = 8;
        String msg;
        if (x > 10) {
            msg = "big";
        } else if (x > 5) {
            msg = "medium";
        } else {
            msg = "small";
        }
        System.out.println(msg);
    }
}
