public class Main {
    static int[] nums = new int[]{1, 2, 3};
    static java.util.List<int> result = new java.util.ArrayList<int>() {{ java.util.ArrayList<int> _tmp = new java.util.ArrayList<>(); for (var n : nums) { if (n > 1) { _tmp.add((((n.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) % 1 == 0) ? (Object)(int)(n.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()) : (Object)(n.stream().mapToDouble(v -> ((Number)v).doubleValue()).sum()))); } } java.util.ArrayList<int> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((int)list.get(i)); } addAll(_tmp);}};

    public static void main(String[] args) {
        System.out.println(((java.util.List<?>)result).stream().map(String::valueOf).collect(java.util.stream.Collectors.joining(" ")));
    }
}
