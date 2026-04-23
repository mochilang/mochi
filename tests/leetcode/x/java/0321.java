import java.util.*;

public class Main {
    static int[] pick(int[] nums, int k) {
        int drop = nums.length - k;
        int[] stack = new int[nums.length];
        int size = 0;
        for (int x : nums) {
            while (drop > 0 && size > 0 && stack[size - 1] < x) {
                size--;
                drop--;
            }
            stack[size++] = x;
        }
        return Arrays.copyOf(stack, k);
    }

    static boolean greater(int[] a, int i, int[] b, int j) {
        while (i < a.length && j < b.length && a[i] == b[j]) {
            i++;
            j++;
        }
        return j == b.length || (i < a.length && a[i] > b[j]);
    }

    static int[] merge(int[] a, int[] b) {
        int[] out = new int[a.length + b.length];
        int i = 0, j = 0;
        for (int p = 0; p < out.length; p++) {
            if (greater(a, i, b, j)) out[p] = a[i++];
            else out[p] = b[j++];
        }
        return out;
    }

    static int[] maxNumber(int[] nums1, int[] nums2, int k) {
        int[] best = new int[0];
        int start = Math.max(0, k - nums2.length);
        int end = Math.min(k, nums1.length);
        for (int take = start; take <= end; take++) {
            int[] cand = merge(pick(nums1, take), pick(nums2, k - take));
            if (greater(cand, 0, best, 0)) best = cand;
        }
        return best;
    }

    static String fmtList(int[] a) {
        StringBuilder sb = new StringBuilder("[");
        for (int i = 0; i < a.length; i++) {
            if (i > 0) sb.append(',');
            sb.append(a[i]);
        }
        return sb.append(']').toString();
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        if (!sc.hasNextInt()) return;
        int t = sc.nextInt();
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n1 = sc.nextInt();
            int[] nums1 = new int[n1];
            for (int i = 0; i < n1; i++) nums1[i] = sc.nextInt();
            int n2 = sc.nextInt();
            int[] nums2 = new int[n2];
            for (int i = 0; i < n2; i++) nums2[i] = sc.nextInt();
            int k = sc.nextInt();
            if (tc > 0) out.append("\n\n");
            out.append(fmtList(maxNumber(nums1, nums2, k)));
        }
        System.out.print(out);
    }
}
