// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z
// pure_global_fold.mochi
public class PureGlobalFold {
    static int k = 2;
    static int inc(int x) {
        return x + k;
    }
    public static void main(String[] args) {
        System.out.println(inc(3));
    }
}
