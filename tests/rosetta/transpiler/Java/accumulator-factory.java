public class Main {

    static java.util.function.Function<Object,Object> accumulator(Object sum) {
        Object[] store = new Object[]{sum};
        java.util.function.Function<Object,Object> add = (nv) -> {
store[0] = store[0] + nv;
    return store[0];
};
        return add;
    }

    static void main() {
        java.util.function.Function<Object,Object> x = accumulator(1);
        x.apply(5);
        accumulator(3);
        System.out.println(String.valueOf(x.apply(2.3)));
    }
    public static void main(String[] args) {
        main();
    }
}
