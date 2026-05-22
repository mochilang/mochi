public class Main {
    static Data1[] items = new Data1[]{new Data1(1, "a"), new Data1(1, "b"), new Data1(2, "c")};
    static class Data1 {
        int n;
        String v;
        Data1(int n, String v) {
            this.n = n;
            this.v = v;
        }
        boolean containsKey(String k) {
            if (k.equals("n")) return true;
            if (k.equals("v")) return true;
            return false;
        }
    }

    static java.util.List<String> result = new java.util.ArrayList<String>() {{ java.util.ArrayList<String> _tmp = new java.util.ArrayList<>(); for (var i : items) { _tmp.add(((Integer) (i.get("v")))); } java.util.ArrayList<String> list = _tmp; list.sort((a, b) -> {Comparable _va = (Comparable)(a.n); Comparable _vb = (Comparable)(b.n); return _va.compareTo(_vb);}); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((String)list.get(i)); } addAll(_tmp);}};

    public static void main(String[] args) {
        System.out.println(((java.util.List<?>)result).stream().map(String::valueOf).collect(java.util.stream.Collectors.joining(" ")));
    }
}
