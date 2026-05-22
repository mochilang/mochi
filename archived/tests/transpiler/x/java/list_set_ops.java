public class Main {

    public static void main(String[] args) {
        System.out.println(java.util.Arrays.toString(java.util.stream.IntStream.concat(java.util.Arrays.stream(new int[]{1, 2}), java.util.Arrays.stream(new int[]{2, 3})).distinct().toArray()));
        System.out.println(java.util.Arrays.toString(java.util.Arrays.stream(new int[]{1, 2, 3}).filter(v -> java.util.Arrays.stream(new int[]{2}).noneMatch(x -> x == v)).toArray()));
        System.out.println(java.util.Arrays.toString(java.util.Arrays.stream(new int[]{1, 2, 3}).filter(v -> java.util.Arrays.stream(new int[]{2, 4}).anyMatch(x -> x == v)).toArray()));
        System.out.println(java.util.stream.IntStream.concat(java.util.Arrays.stream(new int[]{1, 2}), java.util.Arrays.stream(new int[]{2, 3})).toArray().length);
    }
}
