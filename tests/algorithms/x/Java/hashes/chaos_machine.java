public class Main {
    static double[] K;
    static int t;
    static int size;
    static class Machine {
        double[] buffer;
        double[] params;
        int time;
        Machine(double[] buffer, double[] params, int time) {
            this.buffer = buffer;
            this.params = params;
            this.time = time;
        }
        Machine() {}
        @Override public String toString() {
            return String.format("{'buffer': %s, 'params': %s, 'time': %s}", String.valueOf(buffer), String.valueOf(params), String.valueOf(time));
        }
    }

    static class PullResult {
        int value;
        Machine machine;
        PullResult(int value, Machine machine) {
            this.value = value;
            this.machine = machine;
        }
        PullResult() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'machine': %s}", String.valueOf(value), String.valueOf(machine));
        }
    }

    static Machine machine = null;
    static int i_3 = 0;
    static PullResult res_1;

    static double round_dec(double x, int n) {
        double m10 = 1.0;
        int i = 0;
        while (i < n) {
            m10 = m10 * 10.0;
            i = i + 1;
        }
        double y = x * m10 + 0.5;
        return (1.0 * ((Number)(y)).intValue()) / m10;
    }

    static Machine reset() {
        return new Machine(K, new double[]{0.0, 0.0, 0.0, 0.0, 0.0}, 0);
    }

    static Machine push(Machine m, int seed) {
        double[] buf = ((double[])(m.buffer));
        double[] par = ((double[])(m.params));
        int i_1 = 0;
        while (i_1 < buf.length) {
            double value = buf[i_1];
            double e = (1.0 * seed) / value;
            double next_value = buf[Math.floorMod((i_1 + 1), size)] + e;
            next_value = next_value - (1.0 * ((Number)(next_value)).intValue());
            double r = par[i_1] + e;
            r = r - (1.0 * ((Number)(r)).intValue());
            r = r + 3.0;
buf[i_1] = round_dec(r * next_value * (1.0 - next_value), 10);
par[i_1] = r;
            i_1 = i_1 + 1;
        }
        return new Machine(buf, par, m.time + 1);
    }

    static int xor(int a, int b) {
        int aa = a;
        int bb = b;
        int res = 0;
        int bit = 1;
        while (aa > 0 || bb > 0) {
            int abit = Math.floorMod(aa, 2);
            int bbit = Math.floorMod(bb, 2);
            if (abit != bbit) {
                res = res + bit;
            }
            aa = aa / 2;
            bb = bb / 2;
            bit = bit * 2;
        }
        return res;
    }

    static int xorshift(int x, int y) {
        int xv = x;
        int yv = y;
        xv = xor(xv, yv / 8192);
        yv = xor(yv, xv * 131072);
        xv = xor(xv, yv / 32);
        return xv;
    }

    static PullResult pull(Machine m) {
        double[] buf_1 = ((double[])(m.buffer));
        double[] par_1 = ((double[])(m.params));
        int key = Math.floorMod(m.time, size);
        int i_2 = 0;
        while (i_2 < t) {
            double r_1 = par_1[key];
            double value_1 = buf_1[key];
buf_1[key] = round_dec(r_1 * value_1 * (1.0 - value_1), 10);
            double new_r = (1.0 * m.time) * 0.01 + r_1 * 1.01;
            new_r = new_r - (1.0 * ((Number)(new_r)).intValue());
par_1[key] = new_r + 3.0;
            i_2 = i_2 + 1;
        }
        int x = ((Number)(buf_1[Math.floorMod((key + 2), size)] * 10000000000.0)).intValue();
        int y_1 = ((Number)(buf_1[Math.floorMod((key + size - 2), size)] * 10000000000.0)).intValue();
        Machine new_machine = new Machine(buf_1, par_1, m.time + 1);
        int value_2 = Math.floorMod(xorshift(x, y_1), (int)4294967295L);
        return new PullResult(value_2, new_machine);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            K = ((double[])(new double[]{0.33, 0.44, 0.55, 0.44, 0.33}));
            t = 3;
            size = 5;
            machine = reset();
            i_3 = 0;
            while (i_3 < 100) {
                machine = push(machine, i_3);
                i_3 = i_3 + 1;
            }
            res_1 = pull(machine);
            System.out.println(res_1.value);
            System.out.println(java.util.Arrays.toString(res_1.machine.buffer));
            System.out.println(java.util.Arrays.toString(res_1.machine.params));
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
