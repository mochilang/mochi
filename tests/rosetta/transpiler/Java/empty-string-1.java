public class Main {

    static void main() {
        String s;
        String s2 = "";
        s = "";
        System.out.println((s.equals("")) ? 1 : 0);
        System.out.println(_runeLen(s) == 0 ? 1 : 0);
        System.out.println(!(s.equals("")) ? 1 : 0);
        System.out.println(_runeLen(s) != 0 ? 1 : 0);
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
