public class Main {
    static int[] nums = new int[]{1, 2, 3};
    static java.util.List<int> result = new java.util.ArrayList<int>() {{ for (var n : nums) { if (n > 1) { add(sum(n)); } }}};

    public static void main(String[] args) {
        System.out.println(result);
    }
}
