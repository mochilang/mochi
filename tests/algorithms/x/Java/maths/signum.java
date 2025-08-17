public class Main {

    static long signum(double num) {
        if ((double)(num) < (double)(0.0)) {
            return -1;
        }
        if ((double)(num) > (double)(0.0)) {
            return 1;
        }
        return 0;
    }

    static void test_signum() {
        if ((long)(signum((double)(5.0))) != 1L) {
            throw new RuntimeException(String.valueOf("signum(5) failed"));
        }
        if ((long)(signum((double)(-5.0))) != (long)((-1))) {
            throw new RuntimeException(String.valueOf("signum(-5) failed"));
        }
        if ((long)(signum((double)(0.0))) != 0L) {
            throw new RuntimeException(String.valueOf("signum(0) failed"));
        }
        if ((long)(signum((double)(10.5))) != 1L) {
            throw new RuntimeException(String.valueOf("signum(10.5) failed"));
        }
        if ((long)(signum((double)(-10.5))) != (long)((-1))) {
            throw new RuntimeException(String.valueOf("signum(-10.5) failed"));
        }
        if ((long)(signum((double)(1e-06))) != 1L) {
            throw new RuntimeException(String.valueOf("signum(1e-6) failed"));
        }
        if ((long)(signum((double)(-1e-06))) != (long)((-1))) {
            throw new RuntimeException(String.valueOf("signum(-1e-6) failed"));
        }
        if ((long)(signum((double)(123456789.0))) != 1L) {
            throw new RuntimeException(String.valueOf("signum(123456789) failed"));
        }
        if ((long)(signum((double)(-123456789.0))) != (long)((-1))) {
            throw new RuntimeException(String.valueOf("signum(-123456789) failed"));
        }
    }

    static void main() {
        test_signum();
        System.out.println(signum((double)(12.0)));
        System.out.println(signum((double)(-12.0)));
        System.out.println(signum((double)(0.0)));
    }
    public static void main(String[] args) {
        main();
    }
}
