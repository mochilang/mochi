public class Main {
    static int[] nums = new int[]{1, 2, 3};
    static java.util.List<int> result = new java.util.ArrayList<int>() {{ for (var n : nums) { if (n > 1) { add((((n.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) % 1 == 0) ? (Object)(int)(n.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) : (Object)(n.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()))); } }}};

    public static void main(String[] args) {
        System.out.println(((java.util.List<?>)result).stream().map(String::valueOf).collect(java.util.stream.Collectors.joining(" ")));
    }
}
