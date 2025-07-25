public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int parseIntStr(String str) {
        int i = 0;
        boolean neg = false;
        if ((str.length() > 0 && str.substring(0, 1).equals("-"))) {
            neg = true;
            i = 1;
        }
        int n = 0;
        java.util.Map<String,Integer> digits = new java.util.LinkedHashMap<String, Integer>(java.util.Map.of("0", 0, "1", 1, "2", 2, "3", 3, "4", 4, "5", 5, "6", 6, "7", 7, "8", 8, "9", 9));
        while (i < str.length()) {
            n = n * 10 + (int)(((int)digits.get(str.substring(i, i + 1))));
            i = i + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }

    static void main() {
        int total = 0;
        boolean computer = _now() % 2 == 0;
        System.out.println("Enter q to quit at any time\n");
        if (computer) {
            System.out.println("The computer will choose first");
        } else {
            System.out.println("You will choose first");
        }
        System.out.println("\n\nRunning total is now 0\n\n");
        int round = 1;
        boolean done = false;
        while (!done) {
            System.out.println("ROUND " + String.valueOf(round) + ":\n\n");
            int i = 0;
            while (i < 2 && (!done)) {
                if (computer) {
                    int choice = 0;
                    if (total < 18) {
                        choice = _now() % 3 + 1;
                    } else {
                        choice = 21 - total;
                    }
                    total = total + choice;
                    System.out.println("The computer chooses " + String.valueOf(choice));
                    System.out.println("Running total is now " + String.valueOf(total));
                    if (total == 21) {
                        System.out.println("\nSo, commiserations, the computer has won!");
                        done = true;
                    }
                } else {
                    while (true) {
                        System.out.println("Your choice 1 to 3 : ");
                        String line = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
                        if (((line.equals("q")) || line.equals("Q"))) {
                            System.out.println("OK, quitting the game");
                            done = true;
                            break;
                        }
                        int num = parseIntStr(line);
                        if (num < 1 || num > 3) {
                            if (total + num > 21) {
                                System.out.println("Too big, try again");
                            } else {
                                System.out.println("Out of range, try again");
                            }
                            continue;
                        }
                        if (total + num > 21) {
                            System.out.println("Too big, try again");
                            continue;
                        }
                        total = total + num;
                        System.out.println("Running total is now " + String.valueOf(total));
                        break;
                    }
                    if (total == 21) {
                        System.out.println("\nSo, congratulations, you've won!");
                        done = true;
                    }
                }
                System.out.println("\n");
                computer = !computer;
                i = i + 1;
            }
            round = round + 1;
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
