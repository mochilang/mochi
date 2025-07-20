public class Main {
    static int[] nums = new int[]{1, 2};
    static String[] letters = new String[]{"A", "B"};
    static boolean[] bools = new boolean[]{true, false};
    static java.util.List<Result1> combos = new java.util.ArrayList<Result1>() {{ for (var n : nums) { for (var l : letters) { for (var b : bools) { add(new Result1(n, l, b)); } } }}};
    static class Result1 {
        int n;
        String l;
        boolean b;
        Result1(int n, String l, boolean b) {
            this.n = n;
            this.l = l;
            this.b = b;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- Cross Join of three lists ---");
        for (var c : combos) {
            System.out.println(c.n + " " + c.l + " " + c.b);
        }
    }
}
