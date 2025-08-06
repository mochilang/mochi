public class Main {
    static int[] longest_subsequence(int[] xs) {
        int n = xs.length;
        if (n <= 1) {
            return xs;
        }
        int pivot = xs[0];
        boolean is_found = false;
        int i = 1;
        int[] longest_subseq = ((int[])(new int[]{}));
        while (!is_found && i < n) {
            if (xs[i] < pivot) {
                is_found = true;
                int[] temp_array = ((int[])(java.util.Arrays.copyOfRange(xs, i, n)));
                temp_array = ((int[])(longest_subsequence(((int[])(temp_array)))));
                if (temp_array.length > longest_subseq.length) {
                    longest_subseq = ((int[])(temp_array));
                }
            } else {
                i = i + 1;
            }
        }
        int[] filtered = ((int[])(new int[]{}));
        int j = 1;
        while (j < n) {
            if (xs[j] >= pivot) {
                filtered = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(filtered), java.util.stream.IntStream.of(xs[j])).toArray()));
            }
            j = j + 1;
        }
        int[] candidate = ((int[])(new int[]{}));
        candidate = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(candidate), java.util.stream.IntStream.of(pivot)).toArray()));
        candidate = ((int[])(concat(candidate, longest_subsequence(((int[])(filtered))))));
        if (candidate.length > longest_subseq.length) {
            return candidate;
        } else {
            return longest_subseq;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
        return out;
    }
}
