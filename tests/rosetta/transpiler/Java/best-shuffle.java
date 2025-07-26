public class Main {

    static int nextRand(int seed) {
        return Math.floorMod((seed * 1664525 + 1013904223), 2147483647);
    }

    static Object[] shuffleChars(String s, int seed) {
        String[] chars = new String[]{};
        int i = 0;
        while (i < s.length()) {
            chars = java.util.stream.Stream.concat(java.util.Arrays.stream(chars), java.util.stream.Stream.of(s.substring(i, i + 1))).toArray(String[]::new);
            i = i + 1;
        }
        int sd = seed;
        int idx = chars.length - 1;
        while (idx > 0) {
            sd = nextRand(sd);
            int j = ((Number)(Math.floorMod(sd, (idx + 1)))).intValue();
            String tmp = chars[idx];
chars[idx] = chars[j];
chars[j] = tmp;
            idx = idx - 1;
        }
        String res = "";
        i = 0;
        while (i < chars.length) {
            res = res + chars[i];
            i = i + 1;
        }
        return new Object[]{res, sd};
    }

    static Object[] bestShuffle(String s, int seed) {
        Object[] r = shuffleChars(s, seed);
        Object t = r[0];
        Object sd = r[1];
        String[] arr = new String[]{};
        int i = 0;
        while (i < String.valueOf(t).length()) {
            arr = java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(String.valueOf(t).substring(i, i + 1))).toArray(String[]::new);
            i = i + 1;
        }
        i = 0;
        while (i < arr.length) {
            int j = 0;
            while (j < arr.length) {
                if (i != j && arr[i] != s.substring(j, j + 1) && arr[j] != s.substring(i, i + 1)) {
                    String tmp = arr[i];
arr[i] = arr[j];
arr[j] = tmp;
                    break;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        int count = 0;
        i = 0;
        while (i < arr.length) {
            if ((arr[i].equals(s.substring(i, i + 1)))) {
                count = count + 1;
            }
            i = i + 1;
        }
        String out = "";
        i = 0;
        while (i < arr.length) {
            out = out + arr[i];
            i = i + 1;
        }
        return new Object[]{out, sd, count};
    }

    static void main() {
        String[] ts = new String[]{"abracadabra", "seesaw", "elk", "grrrrrr", "up", "a"};
        int seed = 1;
        int i = 0;
        while (i < ts.length) {
            Object[] r = bestShuffle(String.valueOf(ts[i]), seed);
            Object shuf = r[0];
            seed = ((Number)(r[1])).intValue();
            Object cnt = r[2];
            System.out.println(String.valueOf(ts[i]) + " -> " + String.valueOf(shuf) + " (" + String.valueOf(cnt) + ")");
            i = i + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }
}
