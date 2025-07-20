public class Main {
    static int[] xs = new int[]{1, 2, 3};

    public static void main(String[] args) {
        System.out.println(java.util.Arrays.asList(xs).contains(2));
        System.out.println(!(java.util.Arrays.asList(xs).contains(5)));
    }
}
