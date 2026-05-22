public class Main {
    static Data1[] people = new Data1[]{new Data1("Alice", 30), new Data1("Bob", 25)};
    static class Data1 {
        String name;
        int age;
        Data1(String name, int age) {
            this.name = name;
            this.age = age;
        }
        @Override public String toString() {
            return String.format("{'name': %s, 'age': %s}", name, age);
        }
    }


    static java.util.Map<String,Object> asMap(Object o) {
        if (o instanceof java.util.Map<?,?> mm) {
            java.util.LinkedHashMap<String,Object> m = new java.util.LinkedHashMap<>();
            for (java.util.Map.Entry<?,?> e : mm.entrySet()) m.put(String.valueOf(e.getKey()), e.getValue());
            return m;
        }
        java.util.LinkedHashMap<String,Object> m = new java.util.LinkedHashMap<>();
        for (var f : o.getClass().getDeclaredFields()) { try { f.setAccessible(true); m.put(f.getName(), f.get(o)); } catch (Exception e) { throw new RuntimeException(e); } }
        return m;
    }

    static void saveJsonl(java.util.List<?> list) {
        for (Object obj : list) {
            java.util.Map<String,Object> m = asMap(obj);
            java.util.List<String> parts = new java.util.ArrayList<>();
            for (java.util.Map.Entry<?,?> e : m.entrySet()) {
                Object v = e.getValue();
                if (v instanceof String) {
                    parts.add("\"" + e.getKey() + "\": " + "\"" + v + "\"");
                } else {
                    parts.add("\"" + e.getKey() + "\": " + v);
                }
            }
            System.out.println("{" + String.join(", ", parts) + "}");
        }
    }

    public static void main(String[] args) {
        saveJsonl(java.util.Arrays.asList(people));
    }
}
