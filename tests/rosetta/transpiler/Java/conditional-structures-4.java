public class Main {
    static int fetchSomething() {
        return 0;
    }

    static void doPos(int x) {
    }

    static void doNeg(int x) {
    }

    static void example4() {
        int x = fetchSomething();
        if (x > 0) {
            doPos(x);
        } else {
            doNeg(x);
        }
    }
    public static void main(String[] args) {
    }
}
