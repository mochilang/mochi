public class Main {

    public static void main(String[] args) {
        System.out.println(((new int[]{1, 2, 3}.stream().mapToDouble(v -> ((Number)v).doubleValue()).average().orElse(0)) % 1 == 0 ? (int)(new int[]{1, 2, 3}.stream().mapToDouble(v -> ((Number)v).doubleValue()).average().orElse(0)) : (new int[]{1, 2, 3}.stream().mapToDouble(v -> ((Number)v).doubleValue()).average().orElse(0))));
    }
}
