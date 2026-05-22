public class Main {
    static int x = 3;
    static int y = 4;
    static Data1 m = new Data1(x, y);
    static class Data1 {
        int a;
        int b;
        Data1(int a, int b) {
            this.a = a;
            this.b = b;
        }
        boolean containsKey(String k) {
            if (k.equals("a")) return true;
            if (k.equals("b")) return true;
            return false;
        }
    }


    public static void main(String[] args) {
        System.out.println(m.get("a") + " " + m.get("b"));
    }
}
