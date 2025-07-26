public class Main {

    static String mapString(String s, java.util.function.Function<String,String> f) {
        String out = "";
        int i = 0;
        while (i < s.length()) {
            out = out + String.valueOf(f.apply(s.substring(i, i + 1)));
            i = i + 1;
        }
        return out;
    }

    static void main() {
        java.util.function.Function<String,String> fn = (r) -> (r.equals(" ")) ? "" : r;
        mapString("Spaces removed", fn);
        mapString("Test", (r) -> r.toLowerCase());
        mapString("shift", (r) -> r);
    }
    public static void main(String[] args) {
        main();
    }
}
