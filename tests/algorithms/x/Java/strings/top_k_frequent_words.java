public class Main {
    static class WordCount {
        String word;
        int count;
        WordCount(String word, int count) {
            this.word = word;
            this.count = count;
        }
        WordCount() {}
        @Override public String toString() {
            return String.format("{'word': '%s', 'count': %s}", String.valueOf(word), String.valueOf(count));
        }
    }

    static java.util.Map<String,Integer> freq_map = null;

    static void heapify(WordCount[] arr, int index, int heap_size) {
        int largest = index;
        int left = 2 * index + 1;
        int right = 2 * index + 2;
        if (left < heap_size) {
            WordCount left_item = arr[left];
            WordCount largest_item = arr[largest];
            if (left_item.count > largest_item.count) {
                largest = left;
            }
        }
        if (right < heap_size) {
            WordCount right_item = arr[right];
            WordCount largest_item2 = arr[largest];
            if (right_item.count > largest_item2.count) {
                largest = right;
            }
        }
        if (largest != index) {
            WordCount temp = arr[largest];
arr[largest] = arr[index];
arr[index] = temp;
            heapify(((WordCount[])(arr)), largest, heap_size);
        }
    }

    static void build_max_heap(WordCount[] arr) {
        int i = Math.floorDiv(arr.length, 2) - 1;
        while (i >= 0) {
            heapify(((WordCount[])(arr)), i, arr.length);
            i = i - 1;
        }
    }

    static String[] top_k_frequent_words(String[] words, int k_value) {
        freq_map = new java.util.LinkedHashMap<String, Object>();
        int i_1 = 0;
        while (i_1 < words.length) {
            String w = words[i_1];
            if (((Boolean)(freq_map.containsKey(w)))) {
freq_map.put(w, ((Number)(((Object)(freq_map).get(w)))).intValue() + 1);
            } else {
freq_map.put(w, 1);
            }
            i_1 = i_1 + 1;
        }
        WordCount[] heap = ((WordCount[])(new WordCount[]{}));
        for (var w : freq_map) {
            heap = ((WordCount[])(java.util.stream.Stream.concat(java.util.Arrays.stream(heap), java.util.stream.Stream.of(new WordCount(w, ((Object)(freq_map).get(w))))).toArray(WordCount[]::new)));
        }
        build_max_heap(((WordCount[])(heap)));
        String[] result = ((String[])(new String[]{}));
        int heap_size = heap.length;
        int limit = k_value;
        if (limit > heap_size) {
            limit = heap_size;
        }
        int j = 0;
        while (j < limit) {
            WordCount item = heap[0];
            result = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(item.word)).toArray(String[]::new)));
heap[0] = heap[heap_size - 1];
heap[heap_size - 1] = item;
            heap_size = heap_size - 1;
            heapify(((WordCount[])(heap)), 0, heap_size);
            j = j + 1;
        }
        return result;
    }

    static void main() {
        String[] sample = ((String[])(new String[]{"a", "b", "c", "a", "c", "c"}));
        System.out.println(top_k_frequent_words(((String[])(sample)), 3));
        System.out.println(top_k_frequent_words(((String[])(sample)), 2));
        System.out.println(top_k_frequent_words(((String[])(sample)), 1));
        System.out.println(top_k_frequent_words(((String[])(sample)), 0));
    }
    public static void main(String[] args) {
        freq_map = new java.util.LinkedHashMap<String, Integer>();
        main();
    }
}
