public class Main {
    static int[] xs = new int[]{1, 2, 3};
    static java.util.List<Integer> ys = new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : xs) { if (x % 2 == 1) { _tmp.add(x); } } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}};
    static Data1 m = new Data1(1);
    static class Data1 {
        int a;
        Data1(int a) {
            this.a = a;
        }
        boolean containsKey(String k) {
            if (k.equals("a")) return true;
            return false;
        }
    }

    static String s = "hello";

    public static void main(String[] args) {
        System.out.println(ys.contains(1));
        System.out.println(ys.contains(2));
        System.out.println(m.containsKey("a"));
        System.out.println(m.containsKey("b"));
        System.out.println(s.contains("ell"));
        System.out.println(s.contains("foo"));
    }
}
