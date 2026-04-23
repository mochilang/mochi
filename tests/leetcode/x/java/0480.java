import java.io.*;
import java.util.*;

class Main {
    static class DualHeap {
        PriorityQueue<Integer> small = new PriorityQueue<>(Collections.reverseOrder());
        PriorityQueue<Integer> large = new PriorityQueue<>();
        HashMap<Integer, Integer> delayed = new HashMap<>();
        int k, smallSize = 0, largeSize = 0;

        DualHeap(int k) { this.k = k; }

        void pruneSmall() {
            while (!small.isEmpty()) {
                int x = small.peek();
                Integer c = delayed.get(x);
                if (c == null || c == 0) break;
                if (c == 1) delayed.remove(x); else delayed.put(x, c - 1);
                small.poll();
            }
        }

        void pruneLarge() {
            while (!large.isEmpty()) {
                int x = large.peek();
                Integer c = delayed.get(x);
                if (c == null || c == 0) break;
                if (c == 1) delayed.remove(x); else delayed.put(x, c - 1);
                large.poll();
            }
        }

        void rebalance() {
            if (smallSize > largeSize + 1) {
                large.offer(small.poll());
                smallSize--;
                largeSize++;
                pruneSmall();
            } else if (smallSize < largeSize) {
                small.offer(large.poll());
                smallSize++;
                largeSize--;
                pruneLarge();
            }
        }

        void add(int x) {
            if (small.isEmpty() || x <= small.peek()) {
                small.offer(x);
                smallSize++;
            } else {
                large.offer(x);
                largeSize++;
            }
            rebalance();
        }

        void remove(int x) {
            delayed.put(x, delayed.getOrDefault(x, 0) + 1);
            if (x <= small.peek()) {
                smallSize--;
                if (x == small.peek()) pruneSmall();
            } else {
                largeSize--;
                if (!large.isEmpty() && x == large.peek()) pruneLarge();
            }
            rebalance();
        }

        long median2() {
            pruneSmall();
            pruneLarge();
            if ((k & 1) == 1) return (long)small.peek() * 2;
            return (long)small.peek() + large.peek();
        }
    }

    static String fmtVal(long v2) {
        if ((v2 & 1) == 0) return Long.toString(v2 / 2);
        String sign = v2 < 0 ? "-" : "";
        long a = Math.abs(v2);
        return sign + (a / 2) + ".5";
    }

    static String solve(int[] nums, int k) {
        DualHeap dh = new DualHeap(k);
        for (int i = 0; i < k; i++) dh.add(nums[i]);
        ArrayList<String> out = new ArrayList<>();
        out.add(fmtVal(dh.median2()));
        for (int i = k; i < nums.length; i++) {
            dh.add(nums[i]);
            dh.remove(nums[i - k]);
            out.add(fmtVal(dh.median2()));
        }
        return "[" + String.join(",", out) + "]";
    }

    public static void main(String[] args) throws Exception {
        String text = new String(System.in.readAllBytes()).trim();
        if (text.isEmpty()) return;
        String[] v = text.split("\\s+");
        int idx = 0, t = Integer.parseInt(v[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(v[idx++]);
            int k = Integer.parseInt(v[idx++]);
            int[] nums = new int[n];
            for (int i = 0; i < n; i++) nums[i] = Integer.parseInt(v[idx++]);
            if (tc > 0) out.append("\n\n");
            out.append(solve(nums, k));
        }
        System.out.print(out);
    }
}
