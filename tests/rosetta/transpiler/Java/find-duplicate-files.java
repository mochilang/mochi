public class Main {

    static String[][] findDuplicates(java.util.Map<String,String> fs, String[] paths) {
        java.util.Map<String,String> seen = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>()));
        String[][] dups = new String[][]{};
        for (String path : paths) {
            String content = ((String)(fs).get(path));
            if (((Boolean)(seen.containsKey(content)))) {
                dups = appendObj(dups, new String[]{((String)(seen).get(content)), path});
            } else {
seen.put(content, path);
            }
        }
        return dups;
    }

    static void main() {
        java.util.Map<String,String> fs = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("a.txt", "hello"), java.util.Map.entry("b.txt", "world"), java.util.Map.entry("c.txt", "hello"), java.util.Map.entry("d.txt", "foo"), java.util.Map.entry("e.txt", "world")))));
        String[] paths = new String[]{"a.txt", "b.txt", "c.txt", "d.txt", "e.txt"};
        String[][] dups_1 = findDuplicates(fs, paths);
        for (String[] pair : dups_1) {
            System.out.println(pair[0] + " <==> " + pair[1]);
        }
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
