// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
// averages-median-2.mochi
import java.util.*;

public class AveragesMedian2 {
    static double sel(List<? extends Number> list, int k) {
        int i = 0;
        while (i <= k) {
            int minIndex = i;
            int j = i + 1;
            while (j < list.size()) {
                if (String.valueOf(list.get(j)).compareTo(String.valueOf(list.get(minIndex))) < 0) {
                    minIndex = (int)(j);
                }
                j = (int)(j + 1);
            }
            List<? extends Number> tmp = list.get(i);
            list.set(i, list.get(minIndex));
            list.set(minIndex, tmp);
            i = (int)(i + 1);
        }
        return list.get(k);
    }
    static double median(List<? extends Number> a) {
        List<? extends Number> arr = a;
        int half = Integer.parseInt((arr.size() / 2));
        double med = sel(arr, half);
        if (Objects.equals(arr.size() % 2, 0)) {
            return ((Number)(med + ((Number)arr.get(half - 1)).doubleValue())).doubleValue() / 2.000000;
        }
        return med;
    }
    public static void main(String[] args) {
        System.out.println(String.valueOf(median(Arrays.asList(3.000000, 1.000000, 4.000000, 1.000000))));
        System.out.println(String.valueOf(median(Arrays.asList(3.000000, 1.000000, 4.000000, 1.000000, 5.000000))));
    }
}
