public class Main {

    public static void main(String[] args) {
        for (int i = 1; i < 101; i++) {
            java.util.Map<Boolean,java.util.Map<Boolean,String>> m = ((java.util.Map<Boolean,java.util.Map<Boolean,String>>)(new java.util.LinkedHashMap<Boolean, java.util.Map<Boolean,String>>(java.util.Map.ofEntries(java.util.Map.entry(false, ((java.util.Map<Boolean,String>)(new java.util.LinkedHashMap<Boolean, String>(java.util.Map.ofEntries(java.util.Map.entry(false, _p(i)), java.util.Map.entry(true, "Fizz")))))), java.util.Map.entry(true, ((java.util.Map<Boolean,String>)(new java.util.LinkedHashMap<Boolean, String>(java.util.Map.ofEntries(java.util.Map.entry(false, "Buzz"), java.util.Map.entry(true, "FizzBuzz"))))))))));
            System.out.println(((String)(((java.util.Map<Boolean,String>)(m).get(Math.floorMod(i, 5) == 0))).get(Math.floorMod(i, 3) == 0)));
        }
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
