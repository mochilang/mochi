public class Main {

    static int abs_int(int x) {
        if (x < 0) {
            return -x;
        }
        return x;
    }

    static int gcd_iter(int a, int b) {
        int x = abs_int(a);
        int y = abs_int(b);
        while (y != 0) {
            int t = y;
            y = Math.floorMod(x, y);
            x = t;
        }
        return x;
    }

    static boolean is_prime(int n) {
        if (n <= 1) {
            return false;
        }
        int d = 2;
        while (d * d <= n) {
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 1;
        }
        return true;
    }

    static int[] sieve_er(int n) {
        int[] nums = ((int[])(new int[]{}));
        int i = 2;
        while (i <= n) {
            nums = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(nums), java.util.stream.IntStream.of(i)).toArray()));
            i = i + 1;
        }
        int idx = 0;
        while (idx < nums.length) {
            int j = idx + 1;
            while (j < nums.length) {
                if (nums[idx] != 0) {
                    if (Math.floorMod(nums[j], nums[idx]) == 0) {
nums[j] = 0;
                    }
                }
                j = j + 1;
            }
            idx = idx + 1;
        }
        int[] res = ((int[])(new int[]{}));
        int k = 0;
        while (k < nums.length) {
            int v = nums[k];
            if (v != 0) {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(v)).toArray()));
            }
            k = k + 1;
        }
        return res;
    }

    static int[] get_prime_numbers(int n) {
        int[] ans = ((int[])(new int[]{}));
        int num = 2;
        while (num <= n) {
            if (((Boolean)(is_prime(num)))) {
                ans = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ans), java.util.stream.IntStream.of(num)).toArray()));
            }
            num = num + 1;
        }
        return ans;
    }

    static int[] prime_factorization(int number) {
        if (number == 0) {
            return new int[]{0};
        }
        if (number == 1) {
            return new int[]{1};
        }
        int[] ans_1 = ((int[])(new int[]{}));
        if (((Boolean)(is_prime(number)))) {
            ans_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ans_1), java.util.stream.IntStream.of(number)).toArray()));
            return ans_1;
        }
        int quotient = number;
        int factor = 2;
        while (quotient != 1) {
            if (((Boolean)(is_prime(factor))) && Math.floorMod(quotient, factor) == 0) {
                ans_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ans_1), java.util.stream.IntStream.of(factor)).toArray()));
                quotient = quotient / factor;
            } else {
                factor = factor + 1;
            }
        }
        return ans_1;
    }

    static int greatest_prime_factor(int number) {
        int[] factors = ((int[])(prime_factorization(number)));
        int m = factors[0];
        int i_1 = 1;
        while (i_1 < factors.length) {
            if (factors[i_1] > m) {
                m = factors[i_1];
            }
            i_1 = i_1 + 1;
        }
        return m;
    }

    static int smallest_prime_factor(int number) {
        int[] factors_1 = ((int[])(prime_factorization(number)));
        int m_1 = factors_1[0];
        int i_2 = 1;
        while (i_2 < factors_1.length) {
            if (factors_1[i_2] < m_1) {
                m_1 = factors_1[i_2];
            }
            i_2 = i_2 + 1;
        }
        return m_1;
    }

    static int kg_v(int number1, int number2) {
        if (number1 < 1 || number2 < 1) {
            throw new RuntimeException(String.valueOf("numbers must be positive"));
        }
        int g = gcd_iter(number1, number2);
        return (number1 / g) * number2;
    }

    static boolean is_even(int number) {
        return Math.floorMod(number, 2) == 0;
    }

    static boolean is_odd(int number) {
        return Math.floorMod(number, 2) != 0;
    }

    static int[] goldbach(int number) {
        if (!(Boolean)is_even(number) || number <= 2) {
            throw new RuntimeException(String.valueOf("number must be even and > 2"));
        }
        int[] primes = ((int[])(get_prime_numbers(number)));
        int i_3 = 0;
        while (i_3 < primes.length) {
            int j_1 = i_3 + 1;
            while (j_1 < primes.length) {
                if (primes[i_3] + primes[j_1] == number) {
                    return new int[]{primes[i_3], primes[j_1]};
                }
                j_1 = j_1 + 1;
            }
            i_3 = i_3 + 1;
        }
        return new int[]{};
    }

    static int get_prime(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n must be non-negative"));
        }
        int index = 0;
        int ans_2 = 2;
        while (index < n) {
            index = index + 1;
            ans_2 = ans_2 + 1;
            while (!(Boolean)is_prime(ans_2)) {
                ans_2 = ans_2 + 1;
            }
        }
        return ans_2;
    }

    static int[] get_primes_between(int p1, int p2) {
        boolean bad1 = !(Boolean)is_prime(p1);
        boolean bad2 = !(Boolean)is_prime(p2);
        if (bad1 || bad2 || p1 >= p2) {
            throw new RuntimeException(String.valueOf("arguments must be prime and p1 < p2"));
        }
        int num_1 = p1 + 1;
        while (num_1 < p2) {
            if (((Boolean)(is_prime(num_1)))) {
                break;
            }
            num_1 = num_1 + 1;
        }
        int[] ans_3 = ((int[])(new int[]{}));
        while (num_1 < p2) {
            ans_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ans_3), java.util.stream.IntStream.of(num_1)).toArray()));
            num_1 = num_1 + 1;
            while (num_1 < p2) {
                if (((Boolean)(is_prime(num_1)))) {
                    break;
                }
                num_1 = num_1 + 1;
            }
        }
        return ans_3;
    }

    static int[] get_divisors(int n) {
        if (n < 1) {
            throw new RuntimeException(String.valueOf("n must be >= 1"));
        }
        int[] ans_4 = ((int[])(new int[]{}));
        int d_1 = 1;
        while (d_1 <= n) {
            if (Math.floorMod(n, d_1) == 0) {
                ans_4 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ans_4), java.util.stream.IntStream.of(d_1)).toArray()));
            }
            d_1 = d_1 + 1;
        }
        return ans_4;
    }

    static boolean is_perfect_number(int number) {
        if (number <= 1) {
            throw new RuntimeException(String.valueOf("number must be > 1"));
        }
        int[] divisors = ((int[])(get_divisors(number)));
        int sum = 0;
        int i_4 = 0;
        while (i_4 < divisors.length - 1) {
            sum = sum + divisors[i_4];
            i_4 = i_4 + 1;
        }
        return sum == number;
    }

    static int[] simplify_fraction(int numerator, int denominator) {
        if (denominator == 0) {
            throw new RuntimeException(String.valueOf("denominator cannot be zero"));
        }
        int g_1 = gcd_iter(abs_int(numerator), abs_int(denominator));
        return new int[]{numerator / g_1, denominator / g_1};
    }

    static int factorial(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n must be >= 0"));
        }
        int ans_5 = 1;
        int i_5 = 1;
        while (i_5 <= n) {
            ans_5 = ans_5 * i_5;
            i_5 = i_5 + 1;
        }
        return ans_5;
    }

    static int fib(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n must be >= 0"));
        }
        if (n <= 1) {
            return 1;
        }
        int tmp = 0;
        int fib1 = 1;
        int ans_6 = 1;
        int i_6 = 0;
        while (i_6 < n - 1) {
            tmp = ans_6;
            ans_6 = ans_6 + fib1;
            fib1 = tmp;
            i_6 = i_6 + 1;
        }
        return ans_6;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(is_prime(97)));
            System.out.println(_p(sieve_er(20)));
            System.out.println(_p(get_prime_numbers(20)));
            System.out.println(_p(prime_factorization(287)));
            System.out.println(_p(greatest_prime_factor(287)));
            System.out.println(_p(smallest_prime_factor(287)));
            System.out.println(_p(kg_v(8, 10)));
            System.out.println(_p(goldbach(28)));
            System.out.println(_p(get_prime(8)));
            System.out.println(_p(get_primes_between(3, 20)));
            System.out.println(_p(get_divisors(28)));
            System.out.println(_p(is_perfect_number(28)));
            System.out.println(_p(simplify_fraction(10, 20)));
            System.out.println(_p(factorial(5)));
            System.out.println(_p(fib(10)));
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

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
