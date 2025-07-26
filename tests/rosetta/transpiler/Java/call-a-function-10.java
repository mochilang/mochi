public class Main {

    static void main() {
        int[] list = new int[]{};
        int a = 1;
        int d = 2;
        int e = 3;
        int i = 4;
        list = java.util.stream.IntStream.concat(java.util.Arrays.stream(list), java.util.stream.IntStream.of(a)).toArray();
        list = java.util.stream.IntStream.concat(java.util.Arrays.stream(list), java.util.stream.IntStream.of(d)).toArray();
        list = java.util.stream.IntStream.concat(java.util.Arrays.stream(list), java.util.stream.IntStream.of(e)).toArray();
        list = java.util.stream.IntStream.concat(java.util.Arrays.stream(list), java.util.stream.IntStream.of(i)).toArray();
        i = list.length;
    }
    public static void main(String[] args) {
        main();
    }
}
