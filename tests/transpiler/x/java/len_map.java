public class Main {
    static class Data1 {
        int a;
        int b;
        Data1(int a, int b) {
            this.a = a;
            this.b = b;
        }
    }


    public static void main(String[] args) {
        System.out.println(new Data1(1, 2).length);
    }
}
