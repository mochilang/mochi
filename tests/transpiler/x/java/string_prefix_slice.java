public class Main {
    static String prefix = "fore";
    static String s1 = "forest";
    static String s2 = "desert";

    public static void main(String[] args) {
        System.out.println((s1.substring(0, prefix.length()).equals(prefix)) ? 1 : 0);
        System.out.println((s2.substring(0, prefix.length()).equals(prefix)) ? 1 : 0);
    }
}
