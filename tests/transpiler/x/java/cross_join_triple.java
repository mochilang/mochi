public class Main {
    static int[] nums = new int[]{1, 2};
    static String[] letters = new String[]{"A", "B"};
    static boolean[] bools = new boolean[]{true, false};
    static java.util.List<Result2> combos = new java.util.ArrayList<Result2>() {{ for (var n : nums) { for (var l : letters) { for (var b : bools) { add(new Result2(n, l, b)); } } }}};
    static class Result2 {
        int n;
        String l;
        boolean b;
        Result2(int n, String l, boolean b) {
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
