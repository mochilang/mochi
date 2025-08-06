public class Main {
    static class CircularQueue {
        int[] data;
        int front;
        int rear;
        int size;
        int capacity;
        CircularQueue(int[] data, int front, int rear, int size, int capacity) {
            this.data = data;
            this.front = front;
            this.rear = rear;
            this.size = size;
            this.capacity = capacity;
        }
        CircularQueue() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'front': %s, 'rear': %s, 'size': %s, 'capacity': %s}", String.valueOf(data), String.valueOf(front), String.valueOf(rear), String.valueOf(size), String.valueOf(capacity));
        }
    }

    static class DequeueResult {
        CircularQueue queue;
        int value;
        DequeueResult(CircularQueue queue, int value) {
            this.queue = queue;
            this.value = value;
        }
        DequeueResult() {}
        @Override public String toString() {
            return String.format("{'queue': %s, 'value': %s}", String.valueOf(queue), String.valueOf(value));
        }
    }


    static CircularQueue create_queue(int capacity) {
        int[] arr = ((int[])(new int[]{}));
        int i = 0;
        while (i < capacity) {
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        return new CircularQueue(arr, 0, 0, 0, capacity);
    }

    static int length(CircularQueue q) {
        return q.size;
    }

    static boolean is_empty(CircularQueue q) {
        return q.size == 0;
    }

    static int front(CircularQueue q) {
        if (((Boolean)(is_empty(q)))) {
            return 0;
        }
        return q.data[q.front];
    }

    static CircularQueue enqueue(CircularQueue q, int value) {
        if (q.size >= q.capacity) {
            throw new RuntimeException(String.valueOf("QUEUE IS FULL"));
        }
        int[] arr_1 = ((int[])(q.data));
arr_1[q.rear] = value;
q.data = arr_1;
q.rear = Math.floorMod((q.rear + 1), q.capacity);
q.size = q.size + 1;
        return q;
    }

    static DequeueResult dequeue(CircularQueue q) {
        if (q.size == 0) {
            throw new RuntimeException(String.valueOf("UNDERFLOW"));
        }
        int value = q.data[q.front];
        int[] arr2 = ((int[])(q.data));
arr2[q.front] = 0;
q.data = arr2;
q.front = Math.floorMod((q.front + 1), q.capacity);
q.size = q.size - 1;
        return new DequeueResult(q, value);
    }

    static void main() {
        CircularQueue q = create_queue(5);
        System.out.println(is_empty(q));
        q = enqueue(q, 10);
        System.out.println(is_empty(q));
        q = enqueue(q, 20);
        q = enqueue(q, 30);
        System.out.println(front(q));
        DequeueResult r = dequeue(q);
        q = r.queue;
        System.out.println(r.value);
        System.out.println(front(q));
        System.out.println(length(q));
    }
    public static void main(String[] args) {
        main();
    }
}
