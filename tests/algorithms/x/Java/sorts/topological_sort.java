public class Main {
    static java.util.Map<String,Object> edges;
    static String[] vertices;

    static String[] topological_sort(String start, java.util.Map<String,Boolean> visited, String[] sort) {
visited.put(start, true);
        Object neighbors = (Object)(((Object)(edges).get(start)));
        int i = 0;
        while (i < String.valueOf(neighbors).length()) {
            Object neighbor = neighbors[i];
            if (!(Boolean)(visited.containsKey(neighbor))) {
                sort = ((String[])(topological_sort((String)(neighbor), visited, ((String[])(sort)))));
            }
            i = i + 1;
        }
        sort = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(sort), java.util.stream.Stream.of(start)).toArray(String[]::new)));
        if (visited.size() != vertices.length) {
            int j = 0;
            while (j < vertices.length) {
                String v = vertices[j];
                if (!(Boolean)(visited.containsKey(v))) {
                    sort = ((String[])(topological_sort(v, visited, ((String[])(sort)))));
                }
                j = j + 1;
            }
        }
        return sort;
    }

    static void main() {
        String[] result = ((String[])(topological_sort("a", ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>())), ((String[])(new String[]{})))));
        System.out.println(_p(result));
    }
    public static void main(String[] args) {
        edges = ((java.util.Map<String,Object>)(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("a", new String[]{"c", "b"}), java.util.Map.entry("b", new String[]{"d", "e"}), java.util.Map.entry("c", new Object[]{}), java.util.Map.entry("d", new Object[]{}), java.util.Map.entry("e", new Object[]{})))));
        vertices = ((String[])(new String[]{"a", "b", "c", "d", "e"}));
        main();
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
