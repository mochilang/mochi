import java.io.*; import java.util.*;
public class Main {
  static List<Integer> solveCase(String s, List<String> words) {
    List<Integer> ans = new ArrayList<>();
    if (words.isEmpty()) return ans;
    int wlen = words.get(0).length(), total = wlen * words.size();
    List<String> target = new ArrayList<>(words); Collections.sort(target);
    for (int i = 0; i + total <= s.length(); i++) {
      List<String> parts = new ArrayList<>();
      for (int j = 0; j < words.size(); j++) parts.add(s.substring(i + j * wlen, i + (j + 1) * wlen));
      Collections.sort(parts);
      if (parts.equals(target)) ans.add(i);
    }
    return ans;
  }
  static String fmtList(List<Integer> arr){ StringBuilder sb=new StringBuilder("["); for(int i=0;i<arr.size();i++){ if(i>0) sb.append(','); sb.append(arr.get(i)); } sb.append(']'); return sb.toString(); }
  public static void main(String[] args) throws Exception { BufferedReader br=new BufferedReader(new InputStreamReader(System.in)); List<String> lines=new ArrayList<>(); String line; while((line=br.readLine())!=null) lines.add(line); if(lines.isEmpty()) return; int idx=0,t=Integer.parseInt(lines.get(idx++).trim()); StringBuilder out=new StringBuilder(); for(int tc=0;tc<t;tc++){ String s=idx<lines.size()?lines.get(idx++):""; int m=idx<lines.size()?Integer.parseInt(lines.get(idx++).trim()):0; List<String> words=new ArrayList<>(); for(int i=0;i<m;i++) words.add(idx<lines.size()?lines.get(idx++):""); out.append(fmtList(solveCase(s,words))); if(tc+1<t) out.append('\n'); } System.out.print(out.toString()); }
}
