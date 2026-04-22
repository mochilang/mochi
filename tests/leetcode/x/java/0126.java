import java.io.*;
import java.util.*;

public class Main {
    static List<List<String>> ladders(String begin, String end, List<String> words) {
        Set<String> wordSet = new HashSet<>(words);
        if (!wordSet.contains(end)) return new ArrayList<>();
        Map<String, List<String>> parents = new HashMap<>();
        Set<String> level = new HashSet<>(); level.add(begin);
        Set<String> visited = new HashSet<>(); visited.add(begin);
        boolean found = false;
        while (!level.isEmpty() && !found) {
            Set<String> next = new HashSet<>();
            List<String> cur = new ArrayList<>(level); Collections.sort(cur);
            for (String word : cur) {
                char[] arr = word.toCharArray();
                for (int i = 0; i < arr.length; i++) {
                    char orig = arr[i];
                    for (char c = 'a'; c <= 'z'; c++) {
                        if (c == orig) continue;
                        arr[i] = c;
                        String nw = new String(arr);
                        if (!wordSet.contains(nw) || visited.contains(nw)) continue;
                        next.add(nw);
                        parents.computeIfAbsent(nw, k -> new ArrayList<>()).add(word);
                        if (nw.equals(end)) found = true;
                    }
                    arr[i] = orig;
                }
            }
            visited.addAll(next);
            level = next;
        }
        List<List<String>> out = new ArrayList<>();
        if (!found) return out;
        List<String> path = new ArrayList<>(); path.add(end);
        backtrack(end, begin, parents, path, out);
        out.sort(Comparator.comparing(a -> String.join("->", a)));
        return out;
    }

    static void backtrack(String word, String begin, Map<String, List<String>> parents, List<String> path, List<List<String>> out) {
        if (word.equals(begin)) {
            List<String> seq = new ArrayList<>();
            for (int i = path.size() - 1; i >= 0; i--) seq.add(path.get(i));
            out.add(seq);
            return;
        }
        List<String> plist = new ArrayList<>(parents.getOrDefault(word, new ArrayList<>()));
        Collections.sort(plist);
        for (String p : plist) {
            path.add(p);
            backtrack(p, begin, parents, path, out);
            path.remove(path.size() - 1);
        }
    }

    static String fmt(List<List<String>> paths) {
        List<String> lines = new ArrayList<>();
        lines.add(Integer.toString(paths.size()));
        for (List<String> p : paths) lines.add(String.join("->", p));
        return String.join("\n", lines);
    }

    public static void main(String[] args) throws Exception {
        List<String> lines = Arrays.asList(new String(System.in.readAllBytes()).split("\\R", -1));
        if (lines.isEmpty() || lines.get(0).trim().isEmpty()) return;
        int tc = Integer.parseInt(lines.get(0).trim()), idx = 1;
        List<String> out = new ArrayList<>();
        for (int t = 0; t < tc; t++) {
            String begin = lines.get(idx++), end = lines.get(idx++);
            int n = Integer.parseInt(lines.get(idx++).trim());
            List<String> words = new ArrayList<>();
            for (int i = 0; i < n; i++) words.add(lines.get(idx++));
            out.add(fmt(ladders(begin, end, words)));
        }
        System.out.print(String.join("\n\n", out));
    }
}
