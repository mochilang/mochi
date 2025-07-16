// averages-median-1.mochi
import java.util.*;

public class AveragesMedian1 {
    static List<Double> sortFloat(List<? extends Number> xs) {
        List<? extends Number> arr = xs;
        int n = arr.size();
        int i = 0;
        while (i < n) {
            int j = 0;
            while (j < n - 1) {
                if (String.valueOf(arr.get(j)).compareTo(String.valueOf(arr.get(j + 1))) > 0) {
                    List<? extends Number> tmp = arr.get(j);
                    arr.set(j, arr.get(j + 1));
                    arr.set(j + 1, tmp);
                }
                j = (int)(j + 1);
            }
            i = (int)(i + 1);
        }
        return arr;
    }
    static double median(List<? extends Number> a) {
        List<Double> arr = sortFloat(a);
        int half = Integer.parseInt((arr.size() / 2));
        List<Double> m = arr.get(half);
        if (Objects.equals(arr.size() % 2, 0)) {
            m = ((Number)(m + ((Number)arr.get(half - 1)).doubleValue())).doubleValue() / 2.000000;
        }
        return m;
    }
    public static void main(String[] args) {
    System.out.println(String.valueOf(median(Arrays.asList(3.000000, 1.000000, 4.000000, 1.000000))));
    System.out.println(String.valueOf(median(Arrays.asList(3.000000, 1.000000, 4.000000, 1.000000, 5.000000))));
    }
}
