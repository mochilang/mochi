public class Main {

    static int weekday(int y, int m, int d) {
        int yy = y;
        int mm = m;
        if (mm < 3) {
            mm = mm + 12;
            yy = yy - 1;
        }
        int k = Math.floorMod(yy, 100);
        int j = ((Number)((yy / 100))).intValue();
        int a = ((Number)(((13 * (mm + 1)) / 5))).intValue();
        int b = ((Number)((k / 4))).intValue();
        int c = ((Number)((j / 4))).intValue();
        return Math.floorMod((d + a + k + b + c + 5 * j), 7);
    }

    static void main() {
        int[] months31 = new int[]{1, 3, 5, 7, 8, 10, 12};
        String[] names = new String[]{"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};
        int count = 0;
        int firstY = 0;
        int firstM = 0;
        int lastY = 0;
        int lastM = 0;
        int[] haveNone = new int[]{};
        System.out.println("Months with five weekends:");
        for (int year = 1900; year < 2101; year++) {
            boolean hasOne = false;
            for (int m : months31) {
                if (weekday(year, m, 1) == 6) {
                    System.out.println("  " + String.valueOf(year) + " " + names[m - 1]);
                    count = count + 1;
                    hasOne = true;
                    lastY = year;
                    lastM = m;
                    if (firstY == 0) {
                        firstY = year;
                        firstM = m;
                    }
                }
            }
            if (!hasOne) {
                haveNone = java.util.stream.IntStream.concat(java.util.Arrays.stream(haveNone), java.util.stream.IntStream.of(year)).toArray();
            }
        }
        System.out.println(String.valueOf(count) + " total");
        System.out.println("");
        System.out.println("First five dates of weekends:");
        for (int i = 0; i < 5; i++) {
            int day = 1 + 7 * i;
            System.out.println("  Friday, " + names[firstM - 1] + " " + String.valueOf(day) + ", " + String.valueOf(firstY));
        }
        System.out.println("Last five dates of weekends:");
        for (int i = 0; i < 5; i++) {
            int day_1 = 1 + 7 * i;
            System.out.println("  Friday, " + names[lastM - 1] + " " + String.valueOf(day_1) + ", " + String.valueOf(lastY));
        }
        System.out.println("");
        System.out.println("Years with no months with five weekends:");
        for (int y : haveNone) {
            System.out.println("  " + String.valueOf(y));
        }
        System.out.println(String.valueOf(haveNone.length) + " total");
    }
    public static void main(String[] args) {
        main();
    }
}
