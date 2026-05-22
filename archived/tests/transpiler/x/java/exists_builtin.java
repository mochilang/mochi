public class Main {
    static int[] data = new int[]{1, 2};
    static boolean flag = exists(new java.util.ArrayList<Integer>() {{ java.util.ArrayList<Integer> _tmp = new java.util.ArrayList<>(); for (var x : data) { if (x == 1) { _tmp.add(x); } } java.util.ArrayList<Integer> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Integer)list.get(i)); } addAll(_tmp);}});

    public static void main(String[] args) {
        System.out.println(flag);
    }
}
