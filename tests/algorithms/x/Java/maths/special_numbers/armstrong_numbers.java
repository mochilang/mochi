public class Main {

    static int pow_int(int base, int exp) {
        int result = 1;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static boolean armstrong_number(int n) {
        if (n < 1) {
            return false;
        }
        int digits = 0;
        int temp = n;
        while (temp > 0) {
            temp = temp / 10;
            digits = digits + 1;
        }
        int total = 0;
        temp = n;
        while (temp > 0) {
            int rem = Math.floorMod(temp, 10);
            total = total + pow_int(rem, digits);
            temp = temp / 10;
        }
        return total == n;
    }

    static boolean pluperfect_number(int n) {
        if (n < 1) {
            return false;
        }
        int[] digit_histogram = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < 10) {
            digit_histogram = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(digit_histogram), java.util.stream.IntStream.of(0)).toArray()));
            i_1 = i_1 + 1;
        }
        int digit_total = 0;
        int temp_1 = n;
        while (temp_1 > 0) {
            int rem_1 = Math.floorMod(temp_1, 10);
digit_histogram[rem_1] = digit_histogram[rem_1] + 1;
            digit_total = digit_total + 1;
            temp_1 = temp_1 / 10;
        }
        int total_1 = 0;
        i_1 = 0;
        while (i_1 < 10) {
            if (digit_histogram[i_1] > 0) {
                total_1 = total_1 + digit_histogram[i_1] * pow_int(i_1, digit_total);
            }
            i_1 = i_1 + 1;
        }
        return total_1 == n;
    }

    static boolean narcissistic_number(int n) {
        if (n < 1) {
            return false;
        }
        int digits_1 = 0;
        int temp_2 = n;
        while (temp_2 > 0) {
            temp_2 = temp_2 / 10;
            digits_1 = digits_1 + 1;
        }
        temp_2 = n;
        int total_2 = 0;
        while (temp_2 > 0) {
            int rem_2 = Math.floorMod(temp_2, 10);
            total_2 = total_2 + pow_int(rem_2, digits_1);
            temp_2 = temp_2 / 10;
        }
        return total_2 == n;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(armstrong_number(371));
            System.out.println(armstrong_number(200));
            System.out.println(pluperfect_number(371));
            System.out.println(pluperfect_number(200));
            System.out.println(narcissistic_number(371));
            System.out.println(narcissistic_number(200));
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
