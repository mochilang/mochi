public class StringPrefixSlice {
    public static void main(String[] args) {
        String prefix = "fore";
        String s1 = "forest";
        System.out.println(s1.substring(0, prefix.length()).equals(prefix));
        String s2 = "desert";
        System.out.println(s2.substring(0, prefix.length()).equals(prefix));
    }
}
