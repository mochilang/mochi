public class Main {
    static class Writer {
        String order;
        int bits;
        int nbits;
        int[] data;
        Writer(String order, int bits, int nbits, int[] data) {
            this.order = order;
            this.bits = bits;
            this.nbits = nbits;
            this.data = data;
        }
        @Override public String toString() {
            return String.format("{'order': '%s', 'bits': %s, 'nbits': %s, 'data': %s}", String.valueOf(order), String.valueOf(bits), String.valueOf(nbits), String.valueOf(data));
        }
    }


    static int pow2(int n) {
        int v = 1;
        int i = 0;
        while (i < n) {
            v = v * 2;
            i = i + 1;
        }
        return v;
    }

    static int lshift(int x, int n) {
        return x * pow2(n);
    }

    static int rshift(int x, int n) {
        return x / pow2(n);
    }

    static Writer NewWriter(String order) {
        return new Writer(order, 0, 0, new int[]{});
    }

    static Writer writeBitsLSB(Writer w, int c, int width) {
w.bits = w.bits + lshift(c, w.nbits);
w.nbits = w.nbits + width;
        while (w.nbits >= 8) {
            int b = Math.floorMod(w.bits, 256);
w.data = java.util.stream.IntStream.concat(java.util.Arrays.stream(w.data), java.util.stream.IntStream.of(b)).toArray();
w.bits = rshift(w.bits, 8);
w.nbits = w.nbits - 8;
        }
        return w;
    }

    static Writer writeBitsMSB(Writer w, int c, int width) {
w.bits = w.bits + lshift(c, 32 - width - w.nbits);
w.nbits = w.nbits + width;
        while (w.nbits >= 8) {
            int b = Math.floorMod(rshift(w.bits, 24), 256);
w.data = java.util.stream.IntStream.concat(java.util.Arrays.stream(w.data), java.util.stream.IntStream.of(b)).toArray();
w.bits = ((Number)((_modPow2(w.bits, 24)))).intValue() * 256;
w.nbits = w.nbits - 8;
        }
        return w;
    }

    static Writer WriteBits(Writer w, int c, int width) {
        if ((w.order.equals("LSB"))) {
            return writeBitsLSB(w, c, width);
        }
        return writeBitsMSB(w, c, width);
    }

    static Writer CloseWriter(Writer w) {
        if (w.nbits > 0) {
            if ((w.order.equals("MSB"))) {
w.bits = rshift(w.bits, 24);
            }
w.data = java.util.stream.IntStream.concat(java.util.Arrays.stream(w.data), java.util.stream.IntStream.of(Math.floorMod(w.bits, 256))).toArray();
        }
w.bits = 0;
w.nbits = 0;
        return w;
    }

    static String toBinary(int n, int bits) {
        String b = "";
        int val = n;
        int i = 0;
        while (i < bits) {
            b = String.valueOf(Math.floorMod(val, 2)) + b;
            val = val / 2;
            i = i + 1;
        }
        return b;
    }

    static String bytesToBits(int[] bs) {
        String out = "[";
        int i = 0;
        while (i < bs.length) {
            out = out + String.valueOf(toBinary(bs[i], 8));
            if (i + 1 < bs.length) {
                out = out + " ";
            }
            i = i + 1;
        }
        out = out + "]";
        return out;
    }

    static void ExampleWriter_WriteBits() {
        Writer bw = NewWriter("MSB");
        bw = WriteBits(bw, 15, 4);
        bw = WriteBits(bw, 0, 1);
        bw = WriteBits(bw, 19, 5);
        bw = CloseWriter(bw);
        System.out.println(bytesToBits(bw.data));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            ExampleWriter_WriteBits();
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

    static int _modPow2(int v, int n) {
        long mask = (1L << n) - 1L;
        return (int)(((long)v) & mask);
    }
}
