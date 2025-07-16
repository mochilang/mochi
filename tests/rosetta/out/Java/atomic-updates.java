// atomic-updates.mochi
import java.util.*;

public class AtomicUpdates {
    static List<Integer> randOrder(int seed, int n) {
        int next = (seed * 1664525 + 1013904223) % 2147483647;
        return Arrays.asList(next, next % n);
    }
    static List<Integer> randChaos(int seed, int n) {
        int next = (seed * 1103515245 + 12345) % 2147483647;
        return Arrays.asList(next, next % n);
    }
    static void main() {
        int nBuckets = 10;
        int initialSum = 1000;
        List<Integer> buckets = Arrays.asList();
        for (int i = 0; i < nBuckets; i++) {
            buckets.add(0);
        }
        int i = nBuckets;
        int dist = initialSum;
        while (i > 0) {
            double v = dist / i;
            i = (int)(i - 1);
            buckets.set(i, v);
            dist = (int)(dist - v);
        }
        int tc0 = 0;
        int tc1 = 0;
        int total = 0;
        int nTicks = 0;
        int seedOrder = 1;
        int seedChaos = 2;
        System.out.println("sum  ---updates---    mean  buckets");
        int t = 0;
        while (t < 5) {
            List<Integer> r = randOrder(seedOrder, nBuckets);
            seedOrder = (int)(r.get(0));
            List<Integer> b1 = r.get(1);
            int b2 = (b1 + 1) % nBuckets;
            List<Integer> v1 = buckets.get(b1);
            List<Integer> v2 = buckets.get(b2);
            if (String.valueOf(v1).compareTo(String.valueOf(v2)) > 0) {
                int a = Integer.parseInt(((v1 - v2) / 2));
                if (a > ((Number)buckets.get(b1)).doubleValue()) {
                    a = (int)(buckets.get(b1));
                }
                buckets.set(b1, ((Number)buckets.get(b1)).doubleValue() - a);
                buckets.set(b2, ((Number)buckets.get(b2)).doubleValue() + a);
            }
            else {
                int a = Integer.parseInt(((v2 - v1) / 2));
                if (a > ((Number)buckets.get(b2)).doubleValue()) {
                    a = (int)(buckets.get(b2));
                }
                buckets.set(b2, ((Number)buckets.get(b2)).doubleValue() - a);
                buckets.set(b1, ((Number)buckets.get(b1)).doubleValue() + a);
            }
            tc0 = (int)(tc0 + 1);
            r = randChaos(seedChaos, nBuckets);
            seedChaos = (int)(r.get(0));
            b1 = r.get(1);
            b2 = (int)((b1 + 1) % nBuckets);
            r = randChaos(seedChaos, ((Number)buckets.get(b1)).doubleValue() + 1);
            seedChaos = (int)(r.get(0));
            List<Integer> amt = r.get(1);
            if (String.valueOf(amt).compareTo(String.valueOf(buckets.get(b1))) > 0) {
                amt = buckets.get(b1);
            }
            buckets.set(b1, ((Number)buckets.get(b1)).doubleValue() - amt);
            buckets.set(b2, ((Number)buckets.get(b2)).doubleValue() + amt);
            tc1 = (int)(tc1 + 1);
            int sum = 0;
            int idx = 0;
            while (idx < nBuckets) {
                sum = (int)(sum + ((Number)buckets.get(idx)).doubleValue());
                idx = (int)(idx + 1);
            }
            total = (int)(total + tc0 + tc1);
            nTicks = (int)(nTicks + 1);
            System.out.println(String.valueOf(sum) + " " + String.valueOf(tc0) + " " + String.valueOf(tc1) + " " + String.valueOf(total / nTicks) + "  " + String.valueOf(buckets));
            tc0 = (int)(0);
            tc1 = (int)(0);
            t = (int)(t + 1);
        }
    }
    static <T> List<T> append(List<T> list, T item) {
        List<T> res = new ArrayList<>(list);
        res.add(item);
        return res;
    }
    public static void main(String[] args) {
    main();
    }
}
