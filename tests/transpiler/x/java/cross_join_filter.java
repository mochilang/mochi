public class Main {
    static int[] nums = new int[]{1, 2, 3};
    static String[] letters = new String[]{"A", "B"};
    static java.util.List<Result2> pairs = new java.util.ArrayList<Result2>() {{ java.util.ArrayList<Result2> _tmp = new java.util.ArrayList<>(); for (var n : nums) { for (var l : letters) { if (n % 2 == 0) { _tmp.add(new Result2(n, l)); } } } java.util.ArrayList<Result2> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Result2)list.get(i)); } addAll(_tmp);}};
    static class Result2 {
        int n;
        String l;
        Result2(int n, String l) {
            this.n = n;
            this.l = l;
        }
        boolean containsKey(String k) {
            if (k.equals("n")) return true;
            if (k.equals("l")) return true;
            return false;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- Even pairs ---");
        for (var p : pairs) {
            System.out.println(p.n + " " + p.l);
        }
    }
}
