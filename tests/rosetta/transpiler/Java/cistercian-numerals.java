public class Main {
    static String[][] n;
    static java.util.Map<Integer,fn():void> draw;
    static int[] numbers;

    static void initN() {
        int i = 0;
        while (i < 15) {
            String[] row = ((String[])(new String[]{}));
            int j = 0;
            while (j < 11) {
                row = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of(" ")).toArray(String[]::new)));
                j = j + 1;
            }
row[5] = "x";
            n = ((String[][])(appendObj(n, row)));
            i = i + 1;
        }
    }

    static void horiz(int c1, int c2, int r) {
        int c = c1;
        while (c <= c2) {
n[r][c] = "x";
            c = c + 1;
        }
    }

    static void verti(int r1, int r2, int c) {
        int r = r1;
        while (r <= r2) {
n[r][c] = "x";
            r = r + 1;
        }
    }

    static void diagd(int c1, int c2, int r) {
        int c_1 = c1;
        while (c_1 <= c2) {
n[r + c_1 - c1][c_1] = "x";
            c_1 = c_1 + 1;
        }
    }

    static void diagu(int c1, int c2, int r) {
        int c_2 = c1;
        while (c_2 <= c2) {
n[r - c_2 + c1][c_2] = "x";
            c_2 = c_2 + 1;
        }
    }

    static void initDraw() {
draw.put(1, () -> horiz(6, 10, 0));
draw.put(2, () -> horiz(6, 10, 4));
draw.put(3, () -> diagd(6, 10, 0));
draw.put(4, () -> diagu(6, 10, 4));
draw.put(5, () -> {
        ((Runnable)(draw).get(1)).apply();
        ((Runnable)(draw).get(4)).apply();
});
        java.util.Map<Integer,fn():void> draw_0 = draw;
draw.put(6, () -> verti(0, 4, 10));
draw.put(7, () -> {
        ((Runnable)(draw).get(1)).apply();
        ((Runnable)(draw).get(6)).apply();
});
        java.util.Map<Integer,fn():void> draw_1 = draw;
draw.put(8, () -> {
        ((Runnable)(draw).get(2)).apply();
        ((Runnable)(draw).get(6)).apply();
});
        java.util.Map<Integer,fn():void> draw_2 = draw;
draw.put(9, () -> {
        ((Runnable)(draw).get(1)).apply();
        ((Runnable)(draw).get(8)).apply();
});
        java.util.Map<Integer,fn():void> draw_3 = draw;
draw.put(10, () -> horiz(0, 4, 0));
draw.put(20, () -> horiz(0, 4, 4));
draw.put(30, () -> diagu(0, 4, 4));
draw.put(40, () -> diagd(0, 4, 0));
draw.put(50, () -> {
        ((Runnable)(draw).get(10)).apply();
        ((Runnable)(draw).get(40)).apply();
});
        java.util.Map<Integer,fn():void> draw_4 = draw;
draw.put(60, () -> verti(0, 4, 0));
draw.put(70, () -> {
        ((Runnable)(draw).get(10)).apply();
        ((Runnable)(draw).get(60)).apply();
});
        java.util.Map<Integer,fn():void> draw_5 = draw;
draw.put(80, () -> {
        ((Runnable)(draw).get(20)).apply();
        ((Runnable)(draw).get(60)).apply();
});
        java.util.Map<Integer,fn():void> draw_6 = draw;
draw.put(90, () -> {
        ((Runnable)(draw).get(10)).apply();
        ((Runnable)(draw).get(80)).apply();
});
        java.util.Map<Integer,fn():void> draw_7 = draw;
draw.put(100, () -> horiz(6, 10, 14));
draw.put(200, () -> horiz(6, 10, 10));
draw.put(300, () -> diagu(6, 10, 14));
draw.put(400, () -> diagd(6, 10, 10));
draw.put(500, () -> {
        ((Runnable)(draw).get(100)).apply();
        ((Runnable)(draw).get(400)).apply();
});
        java.util.Map<Integer,fn():void> draw_8 = draw;
draw.put(600, () -> verti(10, 14, 10));
draw.put(700, () -> {
        ((Runnable)(draw).get(100)).apply();
        ((Runnable)(draw).get(600)).apply();
});
        java.util.Map<Integer,fn():void> draw_9 = draw;
draw.put(800, () -> {
        ((Runnable)(draw).get(200)).apply();
        ((Runnable)(draw).get(600)).apply();
});
        java.util.Map<Integer,fn():void> draw_10 = draw;
draw.put(900, () -> {
        ((Runnable)(draw).get(100)).apply();
        ((Runnable)(draw).get(800)).apply();
});
        java.util.Map<Integer,fn():void> draw_11 = draw;
draw.put(1000, () -> horiz(0, 4, 14));
draw.put(2000, () -> horiz(0, 4, 10));
draw.put(3000, () -> diagd(0, 4, 10));
draw.put(4000, () -> diagu(0, 4, 14));
draw.put(5000, () -> {
        ((Runnable)(draw).get(1000)).apply();
        ((Runnable)(draw).get(4000)).apply();
});
        java.util.Map<Integer,fn():void> draw_12 = draw;
draw.put(6000, () -> verti(10, 14, 0));
draw.put(7000, () -> {
        ((Runnable)(draw).get(1000)).apply();
        ((Runnable)(draw).get(6000)).apply();
});
        java.util.Map<Integer,fn():void> draw_13 = draw;
draw.put(8000, () -> {
        ((Runnable)(draw).get(2000)).apply();
        ((Runnable)(draw).get(6000)).apply();
});
        java.util.Map<Integer,fn():void> draw_14 = draw;
draw.put(9000, () -> {
        ((Runnable)(draw).get(1000)).apply();
        ((Runnable)(draw).get(8000)).apply();
});
        java.util.Map<Integer,fn():void> draw_15 = draw;
    }

    static void printNumeral() {
        int i_1 = 0;
        while (i_1 < 15) {
            String line = "";
            int j_1 = 0;
            while (j_1 < 11) {
                line = line + n[i_1][j_1] + " ";
                j_1 = j_1 + 1;
            }
            System.out.println(line);
            i_1 = i_1 + 1;
        }
        System.out.println("");
    }
    public static void main(String[] args) {
        n = ((String[][])(new String[][]{}));
        draw = ((java.util.Map<Integer,fn():void>)(new java.util.LinkedHashMap<Integer, fn():void>()));
        initDraw();
        numbers = ((int[])(new int[]{0, 1, 20, 300, 4000, 5555, 6789, 9999}));
        for (int number : numbers) {
            initN();
            System.out.println(_p(number) + ":");
            int num = number;
            int thousands = num / 1000;
            num = Math.floorMod(num, 1000);
            int hundreds = num / 100;
            num = Math.floorMod(num, 100);
            int tens = num / 10;
            int ones = Math.floorMod(num, 10);
            if (thousands > 0) {
                ((Runnable)(draw).get(thousands * 1000)).apply();
            }
            if (hundreds > 0) {
                ((Runnable)(draw).get(hundreds * 100)).apply();
            }
            if (tens > 0) {
                ((Runnable)(draw).get(tens * 10)).apply();
            }
            if (ones > 0) {
                ((Runnable)(draw).get(ones)).apply();
            }
            printNumeral();
        }
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
