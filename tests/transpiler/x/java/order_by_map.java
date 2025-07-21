public class Main {
    static Data1[] data = new Data1[]{new Data1(1, 2), new Data1(1, 1), new Data1(0, 5)};
    static class Data1 {
        int a;
        int b;
        Data1(int a, int b) {
            this.a = a;
            this.b = b;
        }
        boolean containsKey(String k) {
            if (k.equals("a")) return true;
            if (k.equals("b")) return true;
            return false;
        }
    }

    static java.util.List<Data1> sorted = new java.util.ArrayList<Data1>() {{ java.util.ArrayList<Data1> _tmp = new java.util.ArrayList<>(); for (var x : data) { _tmp.add(x); } java.util.ArrayList<Data1> list = _tmp; list.sort((a, b) -> {Comparable _va = (Comparable)(new Data2(a.a, a.b)); Comparable _vb = (Comparable)(new Data2(b.a, b.b)); return _va.compareTo(_vb);}); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Data1)list.get(i)); } addAll(_tmp);}};
    static class Data2 {
        int a;
        int b;
        Data2(int a, int b) {
            this.a = a;
            this.b = b;
        }
        boolean containsKey(String k) {
            if (k.equals("a")) return true;
            if (k.equals("b")) return true;
            return false;
        }
    }


    public static void main(String[] args) {
        System.out.println(((java.util.List<?>)sorted).stream().map(String::valueOf).collect(java.util.stream.Collectors.joining(" ")));
    }
}
