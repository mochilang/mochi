public class Main {
    static class NumberContainer {
        java.util.Map<Integer,int[]> numbermap;
        java.util.Map<Integer,Integer> indexmap;
        NumberContainer(java.util.Map<Integer,int[]> numbermap, java.util.Map<Integer,Integer> indexmap) {
            this.numbermap = numbermap;
            this.indexmap = indexmap;
        }
        NumberContainer() {}
        @Override public String toString() {
            return String.format("{'numbermap': %s, 'indexmap': %s}", String.valueOf(numbermap), String.valueOf(indexmap));
        }
    }

    static java.util.Map<Integer,int[]> nm = null;
    static java.util.Map<Integer,Integer> im = null;
    static NumberContainer cont = null;

    static int[] remove_at(int[] xs, int idx) {
        int[] res = ((int[])(new int[]{}));
        int i = 0;
        while (i < xs.length) {
            if (i != idx) {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i])).toArray()));
            }
            i = i + 1;
        }
        return res;
    }

    static int[] insert_at(int[] xs, int idx, int val) {
        int[] res_1 = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < xs.length) {
            if (i_1 == idx) {
                res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(val)).toArray()));
            }
            res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(xs[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        if (idx == xs.length) {
            res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(val)).toArray()));
        }
        return res_1;
    }

    static int[] binary_search_delete(int[] array, int item) {
        int low = 0;
        int high = array.length - 1;
        int[] arr = ((int[])(array));
        while (low <= high) {
            int mid = Math.floorDiv((low + high), 2);
            if (arr[mid] == item) {
                arr = ((int[])(remove_at(((int[])(arr)), mid)));
                return arr;
            } else             if (arr[mid] < item) {
                low = mid + 1;
            } else {
                high = mid - 1;
            }
        }
        System.out.println("ValueError: Either the item is not in the array or the array was unsorted");
        return arr;
    }

    static int[] binary_search_insert(int[] array, int index) {
        int low_1 = 0;
        int high_1 = array.length - 1;
        int[] arr_1 = ((int[])(array));
        while (low_1 <= high_1) {
            Object mid_1 = Math.floorDiv((low_1 + high_1), 2);
            if (arr_1[mid_1] == index) {
                arr_1 = ((int[])(insert_at(((int[])(arr_1)), ((Number)(mid_1)).intValue() + 1, index)));
                return arr_1;
            } else             if (arr_1[mid_1] < index) {
                low_1 = ((Number)(mid_1)).intValue() + 1;
            } else {
                high_1 = ((Number)(mid_1)).intValue() - 1;
            }
        }
        arr_1 = ((int[])(insert_at(((int[])(arr_1)), low_1, index)));
        return arr_1;
    }

    static NumberContainer change(NumberContainer cont, int idx, int num) {
        java.util.Map<Integer,int[]> numbermap = cont.numbermap;
        java.util.Map<Integer,Integer> indexmap = cont.indexmap;
        if (((Boolean)(indexmap.containsKey(idx)))) {
            int old = (int)(((int)(indexmap).getOrDefault(idx, 0)));
            int[] indexes = (int[])(((int[])(numbermap).get(old)));
            if (indexes.length == 1) {
numbermap.put(old, ((int[])(new int[]{})));
            } else {
numbermap.put(old, ((int[])(binary_search_delete(((int[])(indexes)), idx))));
            }
        }
indexmap.put(idx, num);
        if (((Boolean)(numbermap.containsKey(num)))) {
numbermap.put(num, ((int[])(binary_search_insert((int[])(((int[])(numbermap).get(num))), idx))));
        } else {
numbermap.put(num, ((int[])(new int[]{idx})));
        }
        return new NumberContainer(numbermap, indexmap);
    }

    static int find(NumberContainer cont, int num) {
        java.util.Map<Integer,int[]> numbermap_1 = cont.numbermap;
        if (((Boolean)(numbermap_1.containsKey(num)))) {
            int[] arr_2 = (int[])(((int[])(numbermap_1).get(num)));
            if (arr_2.length > 0) {
                return arr_2[0];
            }
        }
        return -1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            nm = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>()));
            im = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
            cont = new NumberContainer(nm, im);
            System.out.println(find(cont, 10));
            cont = change(cont, 0, 10);
            System.out.println(find(cont, 10));
            cont = change(cont, 0, 20);
            System.out.println(find(cont, 10));
            System.out.println(find(cont, 20));
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
