public class Main {
    static class NumberContainer {
        java.util.Map<Long,long[]> numbermap;
        java.util.Map<Long,Long> indexmap;
        NumberContainer(java.util.Map<Long,long[]> numbermap, java.util.Map<Long,Long> indexmap) {
            this.numbermap = numbermap;
            this.indexmap = indexmap;
        }
        NumberContainer() {}
        @Override public String toString() {
            return String.format("{'numbermap': %s, 'indexmap': %s}", String.valueOf(numbermap), String.valueOf(indexmap));
        }
    }

    static java.util.Map<Long,long[]> nm = null;
    static java.util.Map<Long,Long> im = null;
    static NumberContainer cont = null;

    static long[] remove_at(long[] xs, long idx) {
        long[] res = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(xs.length)) {
            if ((long)(i_1) != (long)(idx)) {
                res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of((long)(xs[(int)((long)(i_1))]))).toArray()));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res;
    }

    static long[] insert_at(long[] xs, long idx, long val) {
        long[] res_1 = ((long[])(new long[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(xs.length)) {
            if ((long)(i_3) == (long)(idx)) {
                res_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_1), java.util.stream.LongStream.of((long)(val))).toArray()));
            }
            res_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_1), java.util.stream.LongStream.of((long)(xs[(int)((long)(i_3))]))).toArray()));
            i_3 = (long)((long)(i_3) + 1L);
        }
        if ((long)(idx) == (long)(xs.length)) {
            res_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_1), java.util.stream.LongStream.of((long)(val))).toArray()));
        }
        return res_1;
    }

    static long[] binary_search_delete(long[] array, long item) {
        long low = 0L;
        long high_1 = (long)((long)(array.length) - 1L);
        long[] arr_1 = ((long[])(array));
        while ((long)(low) <= (long)(high_1)) {
            long mid_1 = (long)((long)(((long)(low) + (long)(high_1))) / 2L);
            if ((long)(arr_1[(int)((long)(mid_1))]) == (long)(item)) {
                arr_1 = ((long[])(remove_at(((long[])(arr_1)), (long)(mid_1))));
                return arr_1;
            } else             if ((long)(arr_1[(int)((long)(mid_1))]) < (long)(item)) {
                low = (long)((long)(mid_1) + 1L);
            } else {
                high_1 = (long)((long)(mid_1) - 1L);
            }
        }
        System.out.println("ValueError: Either the item is not in the array or the array was unsorted");
        return arr_1;
    }

    static long[] binary_search_insert(long[] array, long index) {
        long low_1 = 0L;
        long high_3 = (long)((long)(array.length) - 1L);
        long[] arr_3 = ((long[])(array));
        while ((long)(low_1) <= (long)(high_3)) {
            long mid_3 = (long)((long)(((long)(low_1) + (long)(high_3))) / 2L);
            if ((long)(arr_3[(int)((long)(mid_3))]) == (long)(index)) {
                arr_3 = ((long[])(insert_at(((long[])(arr_3)), (long)((long)(mid_3) + 1L), (long)(index))));
                return arr_3;
            } else             if ((long)(arr_3[(int)((long)(mid_3))]) < (long)(index)) {
                low_1 = (long)((long)(mid_3) + 1L);
            } else {
                high_3 = (long)((long)(mid_3) - 1L);
            }
        }
        arr_3 = ((long[])(insert_at(((long[])(arr_3)), (long)(low_1), (long)(index))));
        return arr_3;
    }

    static NumberContainer change(NumberContainer cont, long idx, long num) {
        java.util.Map<Long,long[]> numbermap = cont.numbermap;
        java.util.Map<Long,Long> indexmap_1 = cont.indexmap;
        if (indexmap_1.containsKey(idx)) {
            long old_1 = (long)(((long)(indexmap_1).getOrDefault(idx, 0L)));
            long[] indexes_1 = (long[])(((long[])(numbermap).get(old_1)));
            if ((long)(indexes_1.length) == 1L) {
numbermap.put(old_1, ((long[])(new long[]{})));
            } else {
numbermap.put(old_1, ((long[])(binary_search_delete(((long[])(indexes_1)), (long)(idx)))));
            }
        }
indexmap_1.put(idx, (long)(num));
        if (numbermap.containsKey(num)) {
numbermap.put(num, ((long[])(binary_search_insert((long[])(((long[])(numbermap).get(num))), (long)(idx)))));
        } else {
numbermap.put(num, ((long[])(new long[]{idx})));
        }
        return new NumberContainer(numbermap, indexmap_1);
    }

    static long find(NumberContainer cont, long num) {
        java.util.Map<Long,long[]> numbermap_1 = cont.numbermap;
        if (numbermap_1.containsKey(num)) {
            long[] arr_5 = (long[])(((long[])(numbermap_1).get(num)));
            if ((long)(arr_5.length) > 0L) {
                return arr_5[(int)((long)(0))];
            }
        }
        return -1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            nm = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>()));
            im = ((java.util.Map<Long,Long>)(new java.util.LinkedHashMap<Long, Long>()));
            cont = new NumberContainer(nm, im);
            System.out.println(find(cont, 10L));
            cont = change(cont, 0L, 10L);
            System.out.println(find(cont, 10L));
            cont = change(cont, 0L, 20L);
            System.out.println(find(cont, 10L));
            System.out.println(find(cont, 20L));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
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
}
