public class Main {

    static java.math.BigInteger max_product_subarray(java.math.BigInteger[] numbers) {
        if (new java.math.BigInteger(String.valueOf(numbers.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return java.math.BigInteger.valueOf(0);
        }
        java.math.BigInteger max_till_now_1 = new java.math.BigInteger(String.valueOf(numbers[_idx((numbers).length, 0L)]));
        java.math.BigInteger min_till_now_1 = new java.math.BigInteger(String.valueOf(numbers[_idx((numbers).length, 0L)]));
        java.math.BigInteger max_prod_1 = new java.math.BigInteger(String.valueOf(numbers[_idx((numbers).length, 0L)]));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(numbers.length))) < 0) {
            java.math.BigInteger number_1 = new java.math.BigInteger(String.valueOf(numbers[_idx((numbers).length, ((java.math.BigInteger)(i_1)).longValue())]));
            if (number_1.compareTo(java.math.BigInteger.valueOf(0)) < 0) {
                java.math.BigInteger temp_1 = new java.math.BigInteger(String.valueOf(max_till_now_1));
                max_till_now_1 = new java.math.BigInteger(String.valueOf(min_till_now_1));
                min_till_now_1 = new java.math.BigInteger(String.valueOf(temp_1));
            }
            java.math.BigInteger prod_max_1 = new java.math.BigInteger(String.valueOf(max_till_now_1.multiply(number_1)));
            if (number_1.compareTo(prod_max_1) > 0) {
                max_till_now_1 = new java.math.BigInteger(String.valueOf(number_1));
            } else {
                max_till_now_1 = new java.math.BigInteger(String.valueOf(prod_max_1));
            }
            java.math.BigInteger prod_min_1 = new java.math.BigInteger(String.valueOf(min_till_now_1.multiply(number_1)));
            if (number_1.compareTo(prod_min_1) < 0) {
                min_till_now_1 = new java.math.BigInteger(String.valueOf(number_1));
            } else {
                min_till_now_1 = new java.math.BigInteger(String.valueOf(prod_min_1));
            }
            if (max_till_now_1.compareTo(max_prod_1) > 0) {
                max_prod_1 = new java.math.BigInteger(String.valueOf(max_till_now_1));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(max_prod_1));
    }
    public static void main(String[] args) {
        System.out.println(max_product_subarray(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(2)).negate())), java.math.BigInteger.valueOf(4)}))));
        System.out.println(max_product_subarray(((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(2)).negate())), java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))}))));
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
