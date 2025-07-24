public class Main {

    static int[] divisors(int n) {
        int[] divs = new int[]{1};
        int[] divs2 = new int[]{};
        int i = 2;
        while (i * i <= n) {
            if (n % i == 0) {
                int j = ((Number)((n / i))).intValue();
                divs = java.util.stream.IntStream.concat(java.util.Arrays.stream(divs), java.util.stream.IntStream.of(i)).toArray();
                if (i != j) {
                    divs2 = java.util.stream.IntStream.concat(java.util.Arrays.stream(divs2), java.util.stream.IntStream.of(j)).toArray();
                }
            }
            i = i + 1;
        }
        int j = divs2.length - 1;
        while (j >= 0) {
            divs = java.util.stream.IntStream.concat(java.util.Arrays.stream(divs), java.util.stream.IntStream.of(divs2[j])).toArray();
            j = j - 1;
        }
        return divs;
    }

    static int sum(int[] xs) {
        int tot = 0;
        for (var v : xs) {
            tot = tot + v;
        }
        return tot;
    }

    static String sumStr(int[] xs) {
        String s = "";
        int i = 0;
        while (i < xs.length) {
            s = s + String.valueOf(xs[i]) + " + ";
            i = i + 1;
        }
        return s.substring(0, s.length() - 3);
    }

    static String pad2(int n) {
        String s = String.valueOf(n);
        if (s.length() < 2) {
            return " " + s;
        }
        return s;
    }

    static String pad5(int n) {
        String s = String.valueOf(n);
        while (s.length() < 5) {
            s = " " + s;
        }
        return s;
    }

    static int abundantOdd(int searchFrom, int countFrom, int countTo, boolean printOne) {
        int count = countFrom;
        int n = searchFrom;
        while (count < countTo) {
            int[] divs = divisors(n);
            double tot = java.util.Arrays.stream(divs).sum();
            if (tot > n) {
                count = count + 1;
                if (printOne && count < countTo) {
                    n = n + 2;
                    continue;
                }
                String s = sumStr(divs);
                if (!(Boolean)printOne) {
                    System.out.println(pad2(count) + ". " + pad5(n) + " < " + s + " = " + String.valueOf(tot));
                } else {
                    System.out.println(String.valueOf(n) + " < " + s + " = " + String.valueOf(tot));
                }
            }
            n = n + 2;
        }
        return n;
    }

    static void main() {
        int max = 25;
        System.out.println("The first " + String.valueOf(max) + " abundant odd numbers are:");
        int n = abundantOdd(1, 0, max, false);
        System.out.println("\nThe one thousandth abundant odd number is:");
        abundantOdd(n, max, 1000, true);
        System.out.println("\nThe first abundant odd number above one billion is:");
        abundantOdd(1000000001, 0, 1, true);
    }
    public static void main(String[] args) {
        main();
    }
}
