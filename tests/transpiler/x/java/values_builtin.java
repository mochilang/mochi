public class Main {
    static Data1 m = new Data1(1, 2, 3);
    static class Data1 {
        int a;
        int b;
        int c;
        Data1(int a, int b, int c) {
            this.a = a;
            this.b = b;
            this.c = c;
        }
        boolean containsKey(String k) {
            if (k.equals("a")) return true;
            if (k.equals("b")) return true;
            if (k.equals("c")) return true;
            return false;
        }
    }


    public static void main(String[] args) {
        System.out.println(((java.util.List<?>)java.util.Arrays.asList(m.a, m.b, m.c)).stream().map(String::valueOf).collect(java.util.stream.Collectors.joining(" ")));
    }
}
