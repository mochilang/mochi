import java.io.*;

class Main {
    static int minPatches(int[] nums, int n) {
        long miss = 1; int i = 0, patches = 0;
        while (miss <= n) {
            if (i < nums.length && nums[i] <= miss) miss += nums[i++];
            else { miss += miss; patches++; }
        }
        return patches;
    }
    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner(System.in); Integer tt = fs.nextIntOrNull(); if (tt == null) return;
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < tt; tc++) {
            int size = fs.nextInt(); int[] nums = new int[size]; for (int i = 0; i < size; i++) nums[i] = fs.nextInt(); int n = fs.nextInt();
            if (tc > 0) out.append("\n\n"); out.append(minPatches(nums, n));
        }
        System.out.print(out);
    }
    static class FastScanner { byte[] data; int idx=0; FastScanner(InputStream is) throws IOException { data=is.readAllBytes(); }
        Integer nextIntOrNull(){ skip(); return idx>=data.length?null:nextInt(); }
        int nextInt(){ skip(); int v=0; while(idx<data.length&&data[idx]>' ') v=v*10+data[idx++]-'0'; return v; }
        void skip(){ while(idx<data.length&&data[idx]<=' ') idx++; } }
}
