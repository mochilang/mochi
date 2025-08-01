public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static boolean leapYear(int y) {
        return (Math.floorMod(y, 4) == 0 && Math.floorMod(y, 100) != 0) || (Math.floorMod(y, 400) == 0);
    }

    static int monthDays(int y, int m) {
        int[] days = new int[]{0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
        if (m == 2 && leapYear(y)) {
            return 29;
        }
        return days[m];
    }

    static int zeller(int y, int m, int d) {
        int mm = m;
        int yy = y;
        if (mm < 3) {
            mm = mm + 12;
            yy = yy - 1;
        }
        int K = Math.floorMod(yy, 100);
        int J = yy / 100;
        int h = Math.floorMod((d + (13 * (mm + 1)) / 5 + K + K / 4 + J / 4 + 5 * J), 7);
        return Math.floorMod((h + 6), 7);
    }

    static int lastSunday(int y, int m) {
        int day = monthDays(y, m);
        while (day > 0 && zeller(y, m, day) != 0) {
            day = day - 1;
        }
        return day;
    }

    static String monthName(int m) {
        String[] names = new String[]{"", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};
        return names[m];
    }

    static void main() {
        int year = Integer.parseInt((_scanner.hasNextLine() ? _scanner.nextLine() : ""));
        System.out.println("Last Sundays of each month of " + String.valueOf(year));
        System.out.println("==================================");
        int m = 1;
        while (m <= 12) {
            int day_1 = lastSunday(year, m);
            System.out.println(String.valueOf(monthName(m)) + ": " + String.valueOf(day_1));
            m = m + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }
}
