public class Main {

    static int[] randOrder(int seed, int n) {
        int next = Math.floorMod((seed * 1664525 + 1013904223), 2147483647);
        return new int[]{next, Math.floorMod(next, n)};
    }

    static int[] randChaos(int seed, int n) {
        int next = Math.floorMod((seed * 1103515245 + 12345), 2147483647);
        return new int[]{next, Math.floorMod(next, n)};
    }

    static void main() {
        int nBuckets = 10;
        int initialSum = 1000;
        int[] buckets = new int[]{};
        for (int i = 0; i < nBuckets; i++) {
            buckets = java.util.stream.IntStream.concat(java.util.Arrays.stream(buckets), java.util.stream.IntStream.of(0)).toArray();
        }
        int i = nBuckets;
        int dist = initialSum;
        while (i > 0) {
            int v = dist / i;
            i = i - 1;
buckets[i] = v;
            dist = dist - v;
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
            int[] r = randOrder(seedOrder, nBuckets);
            seedOrder = r[0];
            int b1 = r[1];
            int b2 = Math.floorMod((b1 + 1), nBuckets);
            int v1 = buckets[b1];
            int v2 = buckets[b2];
            if (v1 > v2) {
                int a = ((Number)(((v1 - v2) / 2))).intValue();
                if (a > buckets[b1]) {
                    a = buckets[b1];
                }
buckets[b1] = buckets[b1] - a;
buckets[b2] = buckets[b2] + a;
            } else {
                int a = ((Number)(((v2 - v1) / 2))).intValue();
                if (a > buckets[b2]) {
                    a = buckets[b2];
                }
buckets[b2] = buckets[b2] - a;
buckets[b1] = buckets[b1] + a;
            }
            tc0 = tc0 + 1;
            r = randChaos(seedChaos, nBuckets);
            seedChaos = r[0];
            b1 = r[1];
            b2 = Math.floorMod((b1 + 1), nBuckets);
            r = randChaos(seedChaos, buckets[b1] + 1);
            seedChaos = r[0];
            int amt = r[1];
            if (amt > buckets[b1]) {
                amt = buckets[b1];
            }
buckets[b1] = buckets[b1] - amt;
buckets[b2] = buckets[b2] + amt;
            tc1 = tc1 + 1;
            int sum = 0;
            int idx = 0;
            while (idx < nBuckets) {
                sum = sum + buckets[idx];
                idx = idx + 1;
            }
            total = total + tc0 + tc1;
            nTicks = nTicks + 1;
            System.out.println(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(sum) + " ") + String.valueOf(tc0)) + " ") + String.valueOf(tc1)) + " ") + String.valueOf(total / nTicks)) + "  ") + String.valueOf(buckets));
            tc0 = 0;
            tc1 = 0;
            t = t + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
