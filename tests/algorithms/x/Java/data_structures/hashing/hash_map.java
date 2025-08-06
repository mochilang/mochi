public class Main {
    static class Bucket {
        int state;
        int key;
        int val;
        Bucket(int state, int key, int val) {
            this.state = state;
            this.key = key;
            this.val = val;
        }
        Bucket() {}
        @Override public String toString() {
            return String.format("{'state': %s, 'key': %s, 'val': %s}", String.valueOf(state), String.valueOf(key), String.valueOf(val));
        }
    }

    static class HashMap {
        Bucket[] buckets;
        int len;
        int cap_num;
        int cap_den;
        int initial_size;
        HashMap(Bucket[] buckets, int len, int cap_num, int cap_den, int initial_size) {
            this.buckets = buckets;
            this.len = len;
            this.cap_num = cap_num;
            this.cap_den = cap_den;
            this.initial_size = initial_size;
        }
        HashMap() {}
        @Override public String toString() {
            return String.format("{'buckets': %s, 'len': %s, 'cap_num': %s, 'cap_den': %s, 'initial_size': %s}", String.valueOf(buckets), String.valueOf(len), String.valueOf(cap_num), String.valueOf(cap_den), String.valueOf(initial_size));
        }
    }

    static HashMap hm = null;

    static Bucket[] make_buckets(int n) {
        Bucket[] buckets = new Bucket[0];
        int i = 0;
        while (i < n) {
            buckets = ((Bucket[])(java.util.stream.Stream.concat(java.util.Arrays.stream(buckets), java.util.stream.Stream.of(new Bucket(0, 0, 0))).toArray(Bucket[]::new)));
            i = i + 1;
        }
        return buckets;
    }

    static HashMap hashmap_new(int initial_size) {
        return new HashMap(make_buckets(initial_size), 0, 3, 4, initial_size);
    }

    static int bucket_index(HashMap hm, int key) {
        int ind = Math.floorMod(key, ((Bucket[]) (hm.get("buckets"))).length);
        if (ind < 0) {
            ind = ind + ((Bucket[]) (hm.get("buckets"))).length;
        }
        return ind;
    }

    static int next_index(HashMap hm, int ind) {
        return Math.floorMod((ind + 1), ((Bucket[]) (hm.get("buckets"))).length);
    }

    static boolean try_set(HashMap hm, int ind, int key, int val) {
        Bucket[] buckets_1 = (Bucket[])(((Bucket[]) (hm.get("buckets"))));
        Bucket b = buckets_1[ind];
        if (b.state == 0 || b.state == 2) {
buckets_1[ind] = new Bucket(1, key, val);
hm.put("buckets", buckets_1);
hm.put("len", (int)(((int) (hm.get("len")))) + 1);
            return true;
        }
        if (b.key == key) {
buckets_1[ind] = new Bucket(1, key, val);
hm.put("buckets", buckets_1);
            return true;
        }
        return false;
    }

    static boolean is_full(HashMap hm) {
        int limit = ((Bucket[]) (hm.get("buckets"))).length * (int)(((int) (hm.get("cap_num")))) / (int)(((int) (hm.get("cap_den"))));
        return (int)(((int) (hm.get("len")))) >= limit;
    }

    static boolean is_sparse(HashMap hm) {
        if (((Bucket[]) (hm.get("buckets"))).length <= (int)(((int) (hm.get("initial_size"))))) {
            return false;
        }
        int limit_1 = ((Bucket[]) (hm.get("buckets"))).length * (int)(((int) (hm.get("cap_num")))) / (2 * (int)(((int) (hm.get("cap_den")))));
        return (int)(((int) (hm.get("len")))) < limit_1;
    }

    static void resize(HashMap hm, int new_size) {
        Bucket[] old = (Bucket[])(((Bucket[]) (hm.get("buckets"))));
hm.put("buckets", make_buckets(new_size));
hm.put("len", 0);
        int i_1 = 0;
        while (i_1 < old.length) {
            Bucket it = old[i_1];
            if (it.state == 1) {
                add_item(hm, it.key, it.val);
            }
            i_1 = i_1 + 1;
        }
    }

    static void size_up(HashMap hm) {
        resize(hm, ((Bucket[]) (hm.get("buckets"))).length * 2);
    }

    static void size_down(HashMap hm) {
        resize(hm, ((Bucket[]) (hm.get("buckets"))).length / 2);
    }

    static void add_item(HashMap hm, int key, int val) {
        int ind_1 = bucket_index(hm, key);
        int i_2 = 0;
        while (i_2 < ((Bucket[]) (hm.get("buckets"))).length) {
            if (((Boolean)(try_set(hm, ind_1, key, val)))) {
                break;
            }
            ind_1 = next_index(hm, ind_1);
            i_2 = i_2 + 1;
        }
    }

    static void hashmap_set(HashMap hm, int key, int val) {
        if (((Boolean)(is_full(hm)))) {
            size_up(hm);
        }
        add_item(hm, key, val);
    }

    static int hashmap_get(HashMap hm, int key) {
        Bucket[] buckets_2 = (Bucket[])(((Bucket[]) (hm.get("buckets"))));
        int ind_2 = bucket_index(hm, key);
        int i_3 = 0;
        while (i_3 < buckets_2.length) {
            Bucket it_1 = buckets_2[ind_2];
            if (it_1.state == 0) {
                break;
            }
            if (it_1.state == 1 && it_1.key == key) {
                return it_1.val;
            }
            ind_2 = next_index(hm, ind_2);
            i_3 = i_3 + 1;
        }
        return 0;
    }

    static void hashmap_del(HashMap hm, int key) {
        Bucket[] buckets_3 = (Bucket[])(((Bucket[]) (hm.get("buckets"))));
        int ind_3 = bucket_index(hm, key);
        int i_4 = 0;
        while (i_4 < buckets_3.length) {
            Bucket it_2 = buckets_3[ind_3];
            if (it_2.state == 0) {
                System.out.println("KeyError: " + _p(key));
                return;
            }
            if (it_2.state == 1 && it_2.key == key) {
buckets_3[ind_3] = new Bucket(2, 0, 0);
hm.put("buckets", buckets_3);
hm.put("len", (int)(((int) (hm.get("len")))) - 1);
                break;
            }
            ind_3 = next_index(hm, ind_3);
            i_4 = i_4 + 1;
        }
        if (((Boolean)(is_sparse(hm)))) {
            size_down(hm);
        }
    }

    static int hashmap_len(HashMap hm) {
        return ((int) (hm.get("len")));
    }

    static String hashmap_repr(HashMap hm) {
        String out = "HashMap(";
        boolean first = true;
        int i_5 = 0;
        while (i_5 < ((Bucket[]) (hm.get("buckets"))).length) {
            Bucket b_1 = ((Bucket[]) (hm.get("buckets")))[i_5];
            if (b_1.state == 1) {
                if (!(Boolean)first) {
                    out = out + ", ";
                } else {
                    first = false;
                }
                out = out + _p(b_1.key) + ": " + _p(b_1.val);
            }
            i_5 = i_5 + 1;
        }
        out = out + ")";
        return out;
    }
    public static void main(String[] args) {
        hm = hashmap_new(5);
        hashmap_set(hm, 1, 10);
        hashmap_set(hm, 2, 20);
        hashmap_set(hm, 3, 30);
        System.out.println(hashmap_repr(hm));
        System.out.println(_p(hashmap_get(hm, 2)));
        hashmap_del(hm, 1);
        System.out.println(hashmap_repr(hm));
        System.out.println(_p(hashmap_len(hm)));
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
