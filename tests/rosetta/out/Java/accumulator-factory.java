// accumulator-factory.mochi
import java.util.*;

public class AccumulatorFactory {
    static Object accumulator(any sum) {
        List<any> store = Arrays.asList(sum);
        Object add = nv -> {
            store.set(0, ((Number)store.get(0)).doubleValue() + nv);
            return store.get(0);
        };
        return add;
    }
    static void main() {
        Object x = accumulator(1);
        x(5);
        accumulator(3);
        System.out.println(String.valueOf(x(2.300000)));
    }
    public static void main(String[] args) {
    main();
    }
}
