public class Main {
    static long[] samples = ((long[])(new long[]{4, 11, 22}));

    static long hexagonal(long n) {
        if ((long)(n) < 1L) {
            throw new RuntimeException(String.valueOf("Input must be a positive integer"));
        }
        return (long)(n) * (long)(((long)(2L * (long)(n)) - 1L));
    }
    public static void main(String[] args) {
        for (long s : samples) {
            System.out.println(hexagonal((long)(s)));
        }
    }
}
