public class Main {
    static class HashTable {
        int size_table;
        int[] values;
        double lim_charge;
        HashTable(int size_table, int[] values, double lim_charge) {
            this.size_table = size_table;
            this.values = values;
            this.lim_charge = lim_charge;
        }
        HashTable() {}
        @Override public String toString() {
            return String.format("{'size_table': %s, 'values': %s, 'lim_charge': %s}", String.valueOf(size_table), String.valueOf(values), String.valueOf(lim_charge));
        }
    }

    static HashTable qp;

    static HashTable create_hash_table(int size) {
        Object vals = new Integer[]{};
        int i = 0;
        while (i < size) {
            vals = java.util.stream.Stream.concat(java.util.Arrays.stream(vals), java.util.stream.Stream.of(null)).toArray(Integer[]::new);
            i = i + 1;
        }
        return new HashTable(size, vals, 0.75);
    }

    static int hash_function(HashTable table, int key) {
        return Math.floorMod(key, table.size_table);
    }

    static double balanced_factor(HashTable table) {
        int count = 0;
        int i_1 = 0;
        while (i_1 < table.values.length) {
            if (!(table.values[i_1] == null)) {
                count = count + 1;
            }
            i_1 = i_1 + 1;
        }
        return (((Number)(count)).doubleValue()) / (((Number)(table.size_table)).doubleValue());
    }

    static int collision_resolution(HashTable table, int key) {
        int i_2 = 1;
        int new_key = hash_function(table, key + i_2 * i_2);
        while (!(table.values[new_key] == null) && table.values[new_key] != key) {
            i_2 = i_2 + 1;
            if (balanced_factor(table) >= table.lim_charge) {
                return table.size_table;
            }
            new_key = hash_function(table, key + i_2 * i_2);
        }
        return new_key;
    }

    static void insert_data(HashTable table, int data) {
        int key = hash_function(table, data);
        int[] vals_1 = ((int[])(table.values));
        if ((vals_1[key] == null)) {
vals_1[key] = data;
        } else         if (vals_1[key] == data) {
table.values = vals_1;
            return;
        } else {
            int new_key_1 = collision_resolution(table, key);
            if (new_key_1 < vals_1.length && (vals_1[new_key_1] == null)) {
vals_1[new_key_1] = data;
            }
        }
table.values = vals_1;
    }

    static String int_to_string(int n) {
        if (n == 0) {
            return "0";
        }
        int num = n;
        boolean neg = false;
        if (num < 0) {
            neg = true;
            num = -num;
        }
        String res = "";
        while (num > 0) {
            int digit = Math.floorMod(num, 10);
            String ch = _substr("0123456789", digit, digit + 1);
            res = ch + res;
            num = Math.floorDiv(num, 10);
        }
        if (neg) {
            res = "-" + res;
        }
        return res;
    }

    static String keys_to_string(HashTable table) {
        String result = "{";
        boolean first = true;
        int i_3 = 0;
        while (i_3 < table.values.length) {
            int v = table.values[i_3];
            if (!(v == null)) {
                if (!first) {
                    result = result + ", ";
                }
                result = result + String.valueOf(int_to_string(i_3)) + ": " + String.valueOf(int_to_string(v));
                first = false;
            }
            i_3 = i_3 + 1;
        }
        result = result + "}";
        return result;
    }
    public static void main(String[] args) {
        qp = create_hash_table(8);
        insert_data(qp, 0);
        insert_data(qp, 999);
        insert_data(qp, 111);
        System.out.println(keys_to_string(qp));
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
