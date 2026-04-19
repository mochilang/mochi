import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Main {
    static int ladderLength(String begin, String end, List<String> words) {
        Set<String> wordSet = new HashSet<>(words);
        if (!wordSet.contains(end)) return 0;
        Set<String> level = new HashSet<>();
        Set<String> visited = new HashSet<>();
        level.add(begin);
        visited.add(begin);
        int steps = 1;
        while (!level.isEmpty()) {
            if (level.contains(end)) return steps;
            Set<String> next = new HashSet<>();
            List<String> cur = new ArrayList<>(level);
            Collections.sort(cur);
            for (String word : cur) {
                char[] arr = word.toCharArray();
                for (int i = 0; i < arr.length; i++) {
                    char orig = arr[i];
                    for (char c = 'a'; c <= 'z'; c++) {
                        if (c == orig) continue;
                        arr[i] = c;
                        String nw = new String(arr);
                        if (wordSet.contains(nw) && !visited.contains(nw)) next.add(nw);
                    }
                    arr[i] = orig;
                }
            }
            visited.addAll(next);
            level = next;
            steps++;
        }
        return 0;
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
            String begin = lines.get(idx++);
            String end = lines.get(idx++);
            int n = Integer.parseInt(lines.get(idx++));
            List<String> words = new ArrayList<>();
            for (int i = 0; i < n; i++) words.add(lines.get(idx++));
            out.add(Integer.toString(ladderLength(begin, end, words)));
        }
        System.out.print(String.join("\n\n", out));
    }
}
