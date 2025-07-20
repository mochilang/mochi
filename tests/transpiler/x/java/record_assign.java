public class Main {
    static class Counter {
        int n;
        Counter(int n) {
            this.n = n;
        }
    }


    static Counter c = new Counter(0);

    static void inc(Counter c) {
        c.n = c.n + 1;
    }
    public static void main(String[] args) {
        inc(new Counter(c.n));
        System.out.println(c.n);
    }
}
