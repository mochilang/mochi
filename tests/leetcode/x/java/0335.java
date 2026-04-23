import java.io.*;

class Main {
    static boolean isSelfCrossing(int[] x) {
        for (int i = 3; i < x.length; i++) {
            if (x[i] >= x[i - 2] && x[i - 1] <= x[i - 3]) return true;
            if (i >= 4 && x[i - 1] == x[i - 3] && x[i] + x[i - 4] >= x[i - 2]) return true;
            if (i >= 5 && x[i - 2] >= x[i - 4] && x[i] + x[i - 4] >= x[i - 2] && x[i - 1] <= x[i - 3] && x[i - 1] + x[i - 5] >= x[i - 3]) return true;
        }
        return false;
    }
    public static void main(String[] args) throws Exception { FastScanner fs = new FastScanner(System.in); Integer tt = fs.nextIntOrNull(); if (tt == null) return; StringBuilder out = new StringBuilder(); for (int tc = 0; tc < tt; tc++) { int n = fs.nextInt(); int[] x = new int[n]; for (int i = 0; i < n; i++) x[i] = fs.nextInt(); if (tc > 0) out.append("\n\n"); out.append(isSelfCrossing(x) ? "true" : "false"); } System.out.print(out); }
    static class FastScanner { byte[] data; int idx=0; FastScanner(InputStream is) throws IOException { data=is.readAllBytes(); } Integer nextIntOrNull(){ skip(); return idx>=data.length?null:nextInt(); } int nextInt(){ skip(); int v=0; while(idx<data.length&&data[idx]>' ') v=v*10+data[idx++]-'0'; return v; } void skip(){ while(idx<data.length&&data[idx]<=' ') idx++; } }
}
