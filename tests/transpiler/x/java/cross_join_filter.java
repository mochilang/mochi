public class Main {
    static int[] nums = new int[]{1, 2, 3};
    static String[] letters = new String[]{"A", "B"};
    static java.util.List<Result1> pairs = new java.util.ArrayList<Result1>() {{ for (var n : nums) { for (var l : letters) { if (n % 2 == 0) { add(new Result1(n, l)); } } }}};
    static class Result1 {
        int n;
        String l;
        Result1(int n, String l) {
            this.n = n;
            this.l = l;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- Even pairs ---");
        for (var p : pairs) {
            System.out.println(p.n + " " + p.l);
        }
    }
}
