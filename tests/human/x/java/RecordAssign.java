public class RecordAssign {
    static class Counter {
        int n;
    }
    static void inc(Counter c) {
        c.n = c.n + 1;
    }
    public static void main(String[] args) {
        Counter c = new Counter();
        c.n = 0;
        inc(c);
        System.out.println(c.n);
    }
}
