import java.io.*; import java.util.*;
public class Main {
  static boolean valid(char[][] b,int r,int c,char ch){ for(int i=0;i<9;i++) if(b[r][i]==ch||b[i][c]==ch) return false; int br=(r/3)*3, bc=(c/3)*3; for(int i=br;i<br+3;i++) for(int j=bc;j<bc+3;j++) if(b[i][j]==ch) return false; return true; }
  static boolean solve(char[][] b){ for(int r=0;r<9;r++) for(int c=0;c<9;c++) if(b[r][c]=='.'){ for(char ch='1'; ch<='9'; ch++) if(valid(b,r,c,ch)){ b[r][c]=ch; if(solve(b)) return true; b[r][c]='.'; } return false; } return true; }
  public static void main(String[] args) throws Exception { BufferedReader br=new BufferedReader(new InputStreamReader(System.in)); List<String> lines=new ArrayList<>(); String line; while((line=br.readLine())!=null) lines.add(line); if(lines.isEmpty()) return; int idx=0,t=Integer.parseInt(lines.get(idx++).trim()); StringBuilder out=new StringBuilder(); for(int tc=0;tc<t;tc++){ char[][] b=new char[9][9]; for(int i=0;i<9;i++) b[i]=lines.get(idx++).toCharArray(); solve(b); for(int i=0;i<9;i++){ out.append(new String(b[i])); if(tc+1<t || i<8) out.append('\n'); } } System.out.print(out.toString()); }
}
