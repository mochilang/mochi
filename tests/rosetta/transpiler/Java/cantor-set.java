public class Main {
    static int width = 81;
    static int height = 5;
    static String[] lines = new String[]{};
    static java.util.Map[] stack = new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("start", 0), java.util.Map.entry("len", width), java.util.Map.entry("index", 1)))};

    static String setChar(String s, int idx, String ch) {
        return s.substring(0, idx) + ch + s.substring(idx + 1, s.length());
    }
    public static void main(String[] args) {
        for (int i = 0; i < height; i++) {
            String row = "";
            int j = 0;
            while (j < width) {
                row = row + "*";
                j = j + 1;
            }
            lines = java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of(row)).toArray(String[]::new);
        }
        while (stack.length > 0) {
            java.util.Map<String,Integer> frame = stack[stack.length - 1];
            stack = java.util.Arrays.copyOfRange(stack, 0, stack.length - 1);
            int start = (int)(((int)frame.getOrDefault("start", 0)));
            int lenSeg = (int)(((int)frame.getOrDefault("len", 0)));
            int index = (int)(((int)frame.getOrDefault("index", 0)));
            int seg = ((Number)((lenSeg / 3))).intValue();
            if (seg == 0) {
                continue;
            }
            int i = index;
            while (i < height) {
                int j = start + seg;
                while (j < start + 2 * seg) {
lines[i] = String.valueOf(setChar(lines[i], j, " "));
                    j = j + 1;
                }
                i = i + 1;
            }
            stack = appendObj(stack, new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("start", start), java.util.Map.entry("len", seg), java.util.Map.entry("index", index + 1))));
            stack = appendObj(stack, new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("start", start + seg * 2), java.util.Map.entry("len", seg), java.util.Map.entry("index", index + 1))));
        }
        for (String line : lines) {
            System.out.println(line);
        }
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
