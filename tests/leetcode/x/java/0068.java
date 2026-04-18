import java.io.*; import java.util.*;
public class Main {
  static List<String> justify(List<String> words, int maxWidth) {
    List<String> res = new ArrayList<>(); int i = 0;
    while (i < words.size()) {
      int j = i, total = 0;
      while (j < words.size() && total + words.get(j).length() + (j - i) <= maxWidth) { total += words.get(j).length(); j++; }
      int gaps = j - i - 1; String line;
      if (j == words.size() || gaps == 0) {
        line = String.join(" ", words.subList(i, j));
        line = line + " ".repeat(maxWidth - line.length());
      } else {
        int spaces = maxWidth - total, base = spaces / gaps, extra = spaces % gaps; StringBuilder sb = new StringBuilder();
        for (int k = i; k < j - 1; k++) { sb.append(words.get(k)); sb.append(" ".repeat(base + ((k - i < extra) ? 1 : 0))); }
        sb.append(words.get(j - 1)); line = sb.toString();
      }
      res.add(line); i = j;
    }
    return res;
  }
  public static void main(String[] args) throws Exception {
    BufferedReader br = new BufferedReader(new InputStreamReader(System.in)); List<String> lines = new ArrayList<>(); String line; while ((line = br.readLine()) != null) lines.add(line);
    if (!lines.isEmpty()) { int idx = 0, t = Integer.parseInt(lines.get(idx++)); List<String> out = new ArrayList<>(); for (int tc = 0; tc < t; tc++) { int n = Integer.parseInt(lines.get(idx++)); List<String> words = new ArrayList<>(lines.subList(idx, idx + n)); idx += n; int width = Integer.parseInt(lines.get(idx++)); List<String> ans = justify(words, width); out.add(String.valueOf(ans.size())); for (String s : ans) out.add("|" + s + "|"); if (tc + 1 < t) out.add("="); } System.out.print(String.join("\n", out)); }
  }
}
