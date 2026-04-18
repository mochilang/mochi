import java.io.*; import java.util.*;
public class Main {
  static int solveCase(String s){ ArrayList<Integer> stack=new ArrayList<>(); stack.add(-1); int best=0; for(int i=0;i<s.length();i++){ if(s.charAt(i)=='(') stack.add(i); else { stack.remove(stack.size()-1); if(stack.isEmpty()) stack.add(i); else best=Math.max(best, i-stack.get(stack.size()-1)); } } return best; }
  public static void main(String[] args) throws Exception { BufferedReader br=new BufferedReader(new InputStreamReader(System.in)); List<String> lines=new ArrayList<>(); String line; while((line=br.readLine())!=null) lines.add(line); if(lines.isEmpty()) return; int idx=0,t=Integer.parseInt(lines.get(idx++).trim()); StringBuilder out=new StringBuilder(); for(int tc=0; tc<t; tc++){ int n=idx<lines.size()?Integer.parseInt(lines.get(idx++).trim()):0; String s=n>0 && idx<lines.size()?lines.get(idx++):""; out.append(solveCase(s)); if(tc+1<t) out.append('\n'); } System.out.print(out.toString()); }
}
