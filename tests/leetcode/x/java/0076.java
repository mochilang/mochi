import java.io.*; import java.util.*;
public class Main {
  static String minWindow(String s, String t) {
    int[] need = new int[128]; int missing = t.length();
    for (int i = 0; i < t.length(); i++) need[t.charAt(i)]++;
    int left = 0, bestStart = 0, bestLen = s.length() + 1;
    for (int right = 0; right < s.length(); right++) {
      int c = s.charAt(right);
      if (need[c] > 0) missing--;
      need[c]--;
      while (missing == 0) {
        if (right - left + 1 < bestLen) { bestStart = left; bestLen = right - left + 1; }
        int lc = s.charAt(left);
        need[lc]++;
        if (need[lc] > 0) missing++;
        left++;
      }
    }
    return bestLen > s.length() ? "" : s.substring(bestStart, bestStart + bestLen);
  }
  public static void main(String[] args) throws Exception {
    BufferedReader br = new BufferedReader(new InputStreamReader(System.in)); List<String> lines = new ArrayList<>(); String line; while ((line = br.readLine()) != null) lines.add(line);
    if (!lines.isEmpty()) { int t = Integer.parseInt(lines.get(0)); List<String> out = new ArrayList<>(); for (int i = 0; i < t; i++) out.add(minWindow(lines.get(1 + 2*i), lines.get(2 + 2*i))); System.out.print(String.join("\n", out)); }
  }
}
