public class Main {
    static int[] vals = new int[]{0, 2, 4, 6, 30, 32, 34, 36, 40, 42, 44, 46, 50, 52, 54, 56, 60, 62, 64, 66};
    static int[] billions = new int[]{0, 2, 4, 6};

    static int[] ebanNumbers(int start, int stop) {
        int[] nums = new int[]{};
        for (int b : billions) {
            for (int m : vals) {
                for (int t : vals) {
                    for (int r : vals) {
                        int n = b * 1000000000 + m * 1000000 + t * 1000 + r;
                        if ((n >= start) && (n <= stop)) {
                            nums = java.util.stream.IntStream.concat(java.util.Arrays.stream(nums), java.util.stream.IntStream.of(n)).toArray();
                        }
                    }
                }
            }
        }
        return nums;
    }

    static int countEban(int start, int stop) {
        int count = 0;
        for (int b : billions) {
            for (int m : vals) {
                for (int t : vals) {
                    for (int r : vals) {
                        int n = b * 1000000000 + m * 1000000 + t * 1000 + r;
                        if ((n >= start) && (n <= stop)) {
                            count = count + 1;
                        }
                    }
                }
            }
        }
        return count;
    }

    static void main() {
        Object[][] ranges = new Object[][]{new Object[]{2, 1000, true}, new Object[]{1000, 4000, true}, new Object[]{2, 10000, false}, new Object[]{2, 100000, false}, new Object[]{2, 1000000, false}, new Object[]{2, 10000000, false}, new Object[]{2, 100000000, false}, new Object[]{2, 1000000000, false}};
        for (Object[] rg : ranges) {
            int start = ((int)(rg[0]));
            int stop = ((int)(rg[1]));
            boolean show = ((boolean)(rg[2]));
            if (start == 2) {
                System.out.println("eban numbers up to and including " + String.valueOf(stop) + ":");
            } else {
                System.out.println("eban numbers between " + String.valueOf(start) + " and " + String.valueOf(stop) + " (inclusive):");
            }
            if (show) {
                int[] nums = ebanNumbers(start, stop);
                String line = "";
                int i = 0;
                while (i < nums.length) {
                    line = line + String.valueOf(nums[i]) + " ";
                    i = i + 1;
                }
                if (_runeLen(line) > 0) {
                    System.out.println(_substr(line, 0, _runeLen(line) - 1));
                }
            }
            int c = countEban(start, stop);
            System.out.println("count = " + String.valueOf(c) + "\n");
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
