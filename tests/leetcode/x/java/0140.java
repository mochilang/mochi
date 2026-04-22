import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class Main {
    static List<String> wordBreak(String s, List<String> words) {
        Set<String> wordSet = new HashSet<>(words);
        List<Integer> lengths = new ArrayList<>(new TreeSet<>(words.stream().map(String::length).toList()));
        Map<Integer, List<String>> memo = new HashMap<>();

        class Solver {
            List<String> dfs(int i) {
                if (memo.containsKey(i)) return memo.get(i);
                List<String> out = new ArrayList<>();
                if (i == s.length()) {
                    out.add("");
                } else {
                    for (int length : lengths) {
                        int j = i + length;
                        if (j > s.length()) break;
                        String word = s.substring(i, j);
                        if (wordSet.contains(word)) {
                            for (String tail : dfs(j)) {
                                out.add(tail.isEmpty() ? word : word + " " + tail);
                            }
                        }
                    }
                    Collections.sort(out);
                }
                memo.put(i, out);
                return out;
            }
        }

        return new Solver().dfs(0);
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        List<String> lines = new ArrayList<>();
        String line;
        while ((line = br.readLine()) != null) lines.add(line);
        if (lines.isEmpty()) return;
        int tc = Integer.parseInt(lines.get(0));
        int idx = 1;
        List<String> out = new ArrayList<>();
        for (int t = 0; t < tc; t++) {
            String s = lines.get(idx++);
            int n = Integer.parseInt(lines.get(idx++));
            List<String> words = new ArrayList<>();
            for (int i = 0; i < n; i++) words.add(lines.get(idx++));
            List<String> ans = wordBreak(s, words);
            List<String> block = new ArrayList<>();
            block.add(Integer.toString(ans.size()));
            block.addAll(ans);
            out.add(String.join("\n", block));
        }
        System.out.print(String.join("\n\n", out));
    }
}
