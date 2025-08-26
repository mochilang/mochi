public class Main {
    static class NumberContainer {
        java.util.Map<java.math.BigInteger,java.math.BigInteger[]> numbermap;
        java.util.Map<java.math.BigInteger,java.math.BigInteger> indexmap;
        NumberContainer(java.util.Map<java.math.BigInteger,java.math.BigInteger[]> numbermap, java.util.Map<java.math.BigInteger,java.math.BigInteger> indexmap) {
            this.numbermap = numbermap;
            this.indexmap = indexmap;
        }
        NumberContainer() {}
        @Override public String toString() {
            return String.format("{'numbermap': %s, 'indexmap': %s}", String.valueOf(numbermap), String.valueOf(indexmap));
        }
    }

    static java.util.Map<java.math.BigInteger,java.math.BigInteger[]> nm = null;
    static java.util.Map<java.math.BigInteger,java.math.BigInteger> im = null;
    static NumberContainer cont = null;

    static java.math.BigInteger[] remove_at(java.math.BigInteger[] xs, java.math.BigInteger idx) {
        java.math.BigInteger[] res = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(xs.length))) < 0) {
            if (i_1.compareTo(idx) != 0) {
                res = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(xs[_idx((xs).length, ((java.math.BigInteger)(i_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
            }
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return ((java.math.BigInteger[])(res));
    }

    static java.math.BigInteger[] insert_at(java.math.BigInteger[] xs, java.math.BigInteger idx, java.math.BigInteger val) {
        java.math.BigInteger[] res_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(xs.length))) < 0) {
            if (i_3.compareTo(idx) == 0) {
                res_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(val)).toArray(java.math.BigInteger[]::new)));
            }
            res_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(xs[_idx((xs).length, ((java.math.BigInteger)(i_3)).longValue())])).toArray(java.math.BigInteger[]::new)));
            i_3 = i_3.add(java.math.BigInteger.valueOf(1));
        }
        if (idx.compareTo(new java.math.BigInteger(String.valueOf(xs.length))) == 0) {
            res_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(val)).toArray(java.math.BigInteger[]::new)));
        }
        return ((java.math.BigInteger[])(res_1));
    }

    static java.math.BigInteger[] binary_search_delete(java.math.BigInteger[] array, java.math.BigInteger item) {
        java.math.BigInteger low = java.math.BigInteger.valueOf(0);
        java.math.BigInteger high_1 = new java.math.BigInteger(String.valueOf(array.length)).subtract(java.math.BigInteger.valueOf(1));
        java.math.BigInteger[] arr_1 = ((java.math.BigInteger[])(array));
        while (low.compareTo(high_1) <= 0) {
            java.math.BigInteger mid_1 = (low.add(high_1)).divide(java.math.BigInteger.valueOf(2));
            if (arr_1[_idx((arr_1).length, ((java.math.BigInteger)(mid_1)).longValue())].compareTo(item) == 0) {
                arr_1 = ((java.math.BigInteger[])(remove_at(((java.math.BigInteger[])(arr_1)), mid_1)));
                return ((java.math.BigInteger[])(arr_1));
            } else             if (arr_1[_idx((arr_1).length, ((java.math.BigInteger)(mid_1)).longValue())].compareTo(item) < 0) {
                low = mid_1.add(java.math.BigInteger.valueOf(1));
            } else {
                high_1 = mid_1.subtract(java.math.BigInteger.valueOf(1));
            }
        }
        System.out.println("ValueError: Either the item is not in the array or the array was unsorted");
        return ((java.math.BigInteger[])(arr_1));
    }

    static java.math.BigInteger[] binary_search_insert(java.math.BigInteger[] array, java.math.BigInteger index) {
        java.math.BigInteger low_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger high_3 = new java.math.BigInteger(String.valueOf(array.length)).subtract(java.math.BigInteger.valueOf(1));
        java.math.BigInteger[] arr_3 = ((java.math.BigInteger[])(array));
        while (low_1.compareTo(high_3) <= 0) {
            java.math.BigInteger mid_3 = (low_1.add(high_3)).divide(java.math.BigInteger.valueOf(2));
            if (arr_3[_idx((arr_3).length, ((java.math.BigInteger)(mid_3)).longValue())].compareTo(index) == 0) {
                arr_3 = ((java.math.BigInteger[])(insert_at(((java.math.BigInteger[])(arr_3)), mid_3.add(java.math.BigInteger.valueOf(1)), index)));
                return ((java.math.BigInteger[])(arr_3));
            } else             if (arr_3[_idx((arr_3).length, ((java.math.BigInteger)(mid_3)).longValue())].compareTo(index) < 0) {
                low_1 = mid_3.add(java.math.BigInteger.valueOf(1));
            } else {
                high_3 = mid_3.subtract(java.math.BigInteger.valueOf(1));
            }
        }
        arr_3 = ((java.math.BigInteger[])(insert_at(((java.math.BigInteger[])(arr_3)), low_1, index)));
        return ((java.math.BigInteger[])(arr_3));
    }

    static NumberContainer change(NumberContainer cont, java.math.BigInteger idx, java.math.BigInteger num) {
        java.util.Map<java.math.BigInteger,java.math.BigInteger[]> numbermap = cont.numbermap;
        java.util.Map<java.math.BigInteger,java.math.BigInteger> indexmap_1 = cont.indexmap;
        if (indexmap_1.containsKey(idx)) {
            java.math.BigInteger old_1 = ((java.math.BigInteger)(indexmap_1).get(idx));
            java.math.BigInteger[] indexes_1 = (java.math.BigInteger[])(((java.math.BigInteger[])(numbermap).get(old_1)));
            if (new java.math.BigInteger(String.valueOf(indexes_1.length)).compareTo(java.math.BigInteger.valueOf(1)) == 0) {
numbermap.put(old_1, ((java.math.BigInteger[])(new java.math.BigInteger[]{})));
            } else {
numbermap.put(old_1, ((java.math.BigInteger[])(binary_search_delete(((java.math.BigInteger[])(indexes_1)), idx))));
            }
        }
indexmap_1.put(idx, num);
        if (numbermap.containsKey(num)) {
numbermap.put(num, ((java.math.BigInteger[])(binary_search_insert((java.math.BigInteger[])(((java.math.BigInteger[])(numbermap).get(num))), idx))));
        } else {
numbermap.put(num, ((java.math.BigInteger[])(new java.math.BigInteger[]{idx})));
        }
        return new NumberContainer(numbermap, indexmap_1);
    }

    static java.math.BigInteger find(NumberContainer cont, java.math.BigInteger num) {
        java.util.Map<java.math.BigInteger,java.math.BigInteger[]> numbermap_1 = cont.numbermap;
        if (numbermap_1.containsKey(num)) {
            java.math.BigInteger[] arr_5 = (java.math.BigInteger[])(((java.math.BigInteger[])(numbermap_1).get(num)));
            if (new java.math.BigInteger(String.valueOf(arr_5.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                return arr_5[_idx((arr_5).length, 0L)];
            }
        }
        return (java.math.BigInteger.valueOf(1)).negate();
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            nm = ((java.util.Map<java.math.BigInteger,java.math.BigInteger[]>)(new java.util.LinkedHashMap<java.math.BigInteger, java.math.BigInteger[]>()));
            im = ((java.util.Map<java.math.BigInteger,java.math.BigInteger>)(new java.util.LinkedHashMap<java.math.BigInteger, java.math.BigInteger>()));
            cont = new NumberContainer(nm, im);
            System.out.println(find(cont, java.math.BigInteger.valueOf(10)));
            cont = change(cont, java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(10));
            System.out.println(find(cont, java.math.BigInteger.valueOf(10)));
            cont = change(cont, java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(20));
            System.out.println(find(cont, java.math.BigInteger.valueOf(10)));
            System.out.println(find(cont, java.math.BigInteger.valueOf(20)));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
