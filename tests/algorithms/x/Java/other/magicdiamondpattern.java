public class Main {

    static String floyd(long n) {
        String result = "";
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            long j_1 = 0L;
            while ((long)(j_1) < (long)((long)((long)(n) - (long)(i_1)) - 1L)) {
                result = result + " ";
                j_1 = (long)((long)(j_1) + 1L);
            }
            long k_1 = 0L;
            while ((long)(k_1) < (long)((long)(i_1) + 1L)) {
                result = result + "* ";
                k_1 = (long)((long)(k_1) + 1L);
            }
            result = result + "\n";
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static String reverse_floyd(long n) {
        String result_1 = "";
        long i_3 = (long)(n);
        while ((long)(i_3) > 0L) {
            long j_3 = (long)(i_3);
            while ((long)(j_3) > 0L) {
                result_1 = result_1 + "* ";
                j_3 = (long)((long)(j_3) - 1L);
            }
            result_1 = result_1 + "\n";
            long k_3 = (long)((long)((long)(n) - (long)(i_3)) + 1L);
            while ((long)(k_3) > 0L) {
                result_1 = result_1 + " ";
                k_3 = (long)((long)(k_3) - 1L);
            }
            i_3 = (long)((long)(i_3) - 1L);
        }
        return result_1;
    }

    static String pretty_print(long n) {
        if ((long)(n) <= 0L) {
            return "       ...       ....        nothing printing :(";
        }
        String upper_half_1 = String.valueOf(floyd((long)(n)));
        String lower_half_1 = String.valueOf(reverse_floyd((long)(n)));
        return upper_half_1 + lower_half_1;
    }

    static void main() {
        System.out.println(pretty_print(3L));
        System.out.println(pretty_print(0L));
    }
    public static void main(String[] args) {
        main();
    }
}
