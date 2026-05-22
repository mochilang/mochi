public class Main {
    static int[] a = new int[]{1, 2};

    public static void main(String[] args) {
        System.out.println(java.util.Arrays.toString(java.util.stream.IntStream.concat(java.util.Arrays.stream(a), java.util.stream.IntStream.of(3)).toArray()));
    }
}
