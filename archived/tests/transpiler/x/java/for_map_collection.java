public class Main {
    static Data1 m = new Data1(1, 2);
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
        for (var k : m) {
            System.out.println(k);
        }
    }
}
