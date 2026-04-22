import java.util.*;

public class Main {
    static List<Integer> countSmaller(int[] nums) {
        int n = nums.length;
        int[] counts = new int[n];
        int[] idx = new int[n];
        int[] tmp = new int[n];
        for (int i = 0; i < n; i++) idx[i] = i;
        sort(nums, idx, tmp, counts, 0, n);
        List<Integer> out = new ArrayList<>(n);
        for (int v : counts) out.add(v);
        return out;
    }

    static void sort(int[] nums, int[] idx, int[] tmp, int[] counts, int lo, int hi) {
        if (hi - lo <= 1) return;
        int mid = (lo + hi) / 2;
        sort(nums, idx, tmp, counts, lo, mid);
        sort(nums, idx, tmp, counts, mid, hi);
        int i = lo, j = mid, k = lo, moved = 0;
        while (i < mid && j < hi) {
            if (nums[idx[j]] < nums[idx[i]]) {
                tmp[k++] = idx[j++];
                moved++;
            } else {
                counts[idx[i]] += moved;
                tmp[k++] = idx[i++];
            }
        }
        while (i < mid) {
            counts[idx[i]] += moved;
            tmp[k++] = idx[i++];
        }
        while (j < hi) tmp[k++] = idx[j++];
        for (int p = lo; p < hi; p++) idx[p] = tmp[p];
    }

    static String fmtList(List<Integer> a) {
        StringBuilder sb = new StringBuilder("[");
        for (int i = 0; i < a.size(); i++) {
            if (i > 0) sb.append(',');
            sb.append(a.get(i));
        }
        sb.append(']');
        return sb.toString();
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        if (!sc.hasNextInt()) return;
        int t = sc.nextInt();
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = sc.nextInt();
            int[] nums = new int[n];
            for (int i = 0; i < n; i++) nums[i] = sc.nextInt();
            if (tc > 0) out.append("\n\n");
            out.append(fmtList(countSmaller(nums)));
        }
        System.out.print(out);
    }
}
