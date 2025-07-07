public class MatchExpr {
    public static void main(String[] args) {
        int x = 2;
        String label;
        switch (x) {
            case 1:
                label = "one";
                break;
            case 2:
                label = "two";
                break;
            case 3:
                label = "three";
                break;
            default:
                label = "unknown";
        }
        System.out.println(label);
    }
}
