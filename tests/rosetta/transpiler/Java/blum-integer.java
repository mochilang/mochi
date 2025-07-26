public class Main {

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        if (Math.floorMod(n, 2) == 0) {
            return n == 2;
        }
        if (Math.floorMod(n, 3) == 0) {
            return n == 3;
        }
        int d = 5;
        while (d * d <= n) {
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 2;
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 4;
        }
        return true;
    }

    static int firstPrimeFactor(int n) {
        if (n == 1) {
            return 1;
        }
        if (Math.floorMod(n, 3) == 0) {
            return 3;
        }
        if (Math.floorMod(n, 5) == 0) {
            return 5;
        }
        int[] inc = new int[]{4, 2, 4, 2, 4, 6, 2, 6};
        int k = 7;
        int i = 0;
        while (k * k <= n) {
            if (Math.floorMod(n, k) == 0) {
                return k;
            }
            k = k + inc[i];
            i = Math.floorMod((i + 1), inc.length);
        }
        return n;
    }

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < s.length()) {
            if ((s.substring(i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String padLeft(int n, int width) {
        String s = String.valueOf(n);
        while (s.length() < width) {
            s = " " + s;
        }
        return s;
    }

    static String formatFloat(double f, int prec) {
        String s = String.valueOf(f);
        int idx = ((Number)(s.indexOf("."))).intValue();
        if (idx < 0) {
            return s;
        }
        int need = idx + 1 + prec;
        if (s.length() > need) {
            return s.substring(0, need);
        }
        return s;
    }

    static void main() {
        int[] blum = new int[]{};
        int[] counts = new int[]{0, 0, 0, 0};
        int[] digits = new int[]{1, 3, 7, 9};
        int i = 1;
        int bc = 0;
        while (true) {
            int p = firstPrimeFactor(i);
            if (Math.floorMod(p, 4) == 3) {
                int q = ((Number)((i / p))).intValue();
                if (q != p && Math.floorMod(q, 4) == 3 && isPrime(q)) {
                    if (bc < 50) {
                        blum = java.util.stream.IntStream.concat(java.util.Arrays.stream(blum), java.util.stream.IntStream.of(i)).toArray();
                    }
                    int d = Math.floorMod(i, 10);
                    if (d == 1) {
counts[0] = counts[0] + 1;
                    } else                     if (d == 3) {
counts[1] = counts[1] + 1;
                    } else                     if (d == 7) {
counts[2] = counts[2] + 1;
                    } else                     if (d == 9) {
counts[3] = counts[3] + 1;
                    }
                    bc = bc + 1;
                    if (bc == 50) {
                        System.out.println("First 50 Blum integers:");
                        int idx = 0;
                        while (idx < 50) {
                            String line = "";
                            int j = 0;
                            while (j < 10) {
                                line = line + String.valueOf(padLeft(blum[idx], 3)) + " ";
                                idx = idx + 1;
                                j = j + 1;
                            }
                            System.out.println(line.substring(0, line.length() - 1));
                        }
                        break;
                    }
                }
            }
            if (Math.floorMod(i, 5) == 3) {
                i = i + 4;
            } else {
                i = i + 2;
            }
        }
    }
    public static void main(String[] args) {
        main();
    }
}
