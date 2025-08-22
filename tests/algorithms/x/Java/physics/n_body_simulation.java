public class Main {
    static class Body {
        double position_x;
        double position_y;
        double velocity_x;
        double velocity_y;
        double mass;
        Body(double position_x, double position_y, double velocity_x, double velocity_y, double mass) {
            this.position_x = position_x;
            this.position_y = position_y;
            this.velocity_x = velocity_x;
            this.velocity_y = velocity_y;
            this.mass = mass;
        }
        Body() {}
        @Override public String toString() {
            return String.format("{'position_x': %s, 'position_y': %s, 'velocity_x': %s, 'velocity_y': %s, 'mass': %s}", String.valueOf(position_x), String.valueOf(position_y), String.valueOf(velocity_x), String.valueOf(velocity_y), String.valueOf(mass));
        }
    }

    static class BodySystem {
        Body[] bodies;
        double gravitation_constant;
        double time_factor;
        double softening_factor;
        BodySystem(Body[] bodies, double gravitation_constant, double time_factor, double softening_factor) {
            this.bodies = bodies;
            this.gravitation_constant = gravitation_constant;
            this.time_factor = time_factor;
            this.softening_factor = softening_factor;
        }
        BodySystem() {}
        @Override public String toString() {
            return String.format("{'bodies': %s, 'gravitation_constant': %s, 'time_factor': %s, 'softening_factor': %s}", String.valueOf(bodies), String.valueOf(gravitation_constant), String.valueOf(time_factor), String.valueOf(softening_factor));
        }
    }


    static Body make_body(double px, double py, double vx, double vy, double mass) {
        return new Body(px, py, vx, vy, mass);
    }

    static Body update_velocity(Body body, double force_x, double force_y, double delta_time) {
body.velocity_x = (double)(body.velocity_x) + (double)((double)(force_x) * (double)(delta_time));
body.velocity_y = (double)(body.velocity_y) + (double)((double)(force_y) * (double)(delta_time));
        return body;
    }

    static Body update_position(Body body, double delta_time) {
body.position_x = (double)(body.position_x) + (double)((double)(body.velocity_x) * (double)(delta_time));
body.position_y = (double)(body.position_y) + (double)((double)(body.velocity_y) * (double)(delta_time));
        return body;
    }

    static BodySystem make_body_system(Body[] bodies, double g, double tf, double sf) {
        return new BodySystem(bodies, g, tf, sf);
    }

    static double sqrtApprox(double x) {
        double guess = (double)((double)(x) / (double)(2.0));
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess = (double)((double)(((double)(guess) + (double)((double)(x) / (double)(guess)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess;
    }

    static BodySystem update_system(BodySystem system, double delta_time) {
        Body[] bodies = ((Body[])(system.bodies));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(bodies.length)) {
            Body body1_1 = bodies[(int)((long)(i_3))];
            double force_x_1 = (double)(0.0);
            double force_y_1 = (double)(0.0);
            long j_1 = 0L;
            while ((long)(j_1) < (long)(bodies.length)) {
                if ((long)(i_3) != (long)(j_1)) {
                    Body body2_1 = bodies[(int)((long)(j_1))];
                    double dif_x_1 = (double)((double)(body2_1.position_x) - (double)(body1_1.position_x));
                    double dif_y_1 = (double)((double)(body2_1.position_y) - (double)(body1_1.position_y));
                    double distance_sq_1 = (double)((double)((double)((double)(dif_x_1) * (double)(dif_x_1)) + (double)((double)(dif_y_1) * (double)(dif_y_1))) + (double)(system.softening_factor));
                    double distance_1 = (double)(sqrtApprox((double)(distance_sq_1)));
                    double denom_1 = (double)((double)((double)(distance_1) * (double)(distance_1)) * (double)(distance_1));
                    force_x_1 = (double)((double)(force_x_1) + (double)((double)((double)((double)(system.gravitation_constant) * (double)(body2_1.mass)) * (double)(dif_x_1)) / (double)(denom_1)));
                    force_y_1 = (double)((double)(force_y_1) + (double)((double)((double)((double)(system.gravitation_constant) * (double)(body2_1.mass)) * (double)(dif_y_1)) / (double)(denom_1)));
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            body1_1 = update_velocity(body1_1, (double)(force_x_1), (double)(force_y_1), (double)((double)(delta_time) * (double)(system.time_factor)));
bodies[(int)((long)(i_3))] = body1_1;
            i_3 = (long)((long)(i_3) + 1L);
        }
        i_3 = 0L;
        while ((long)(i_3) < (long)(bodies.length)) {
            Body body_1 = bodies[(int)((long)(i_3))];
            body_1 = update_position(body_1, (double)((double)(delta_time) * (double)(system.time_factor)));
bodies[(int)((long)(i_3))] = body_1;
            i_3 = (long)((long)(i_3) + 1L);
        }
system.bodies = bodies;
        return system;
    }

    static void main() {
        Body b1 = make_body((double)(0.0), (double)(0.0), (double)(0.0), (double)(0.0), (double)(1.0));
        Body b2_1 = make_body((double)(10.0), (double)(0.0), (double)(0.0), (double)(0.0), (double)(1.0));
        BodySystem sys1_1 = make_body_system(((Body[])(new Body[]{b1, b2_1})), (double)(1.0), (double)(1.0), (double)(0.0));
        sys1_1 = update_system(sys1_1, (double)(1.0));
        Body b1_after_1 = sys1_1.bodies[(int)(0L)];
        double pos1x_1 = (double)(b1_after_1.position_x);
        double pos1y_1 = (double)(b1_after_1.position_y);
        json(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("x", (Object)(pos1x_1)), java.util.Map.entry("y", (Object)(pos1y_1)))));
        double vel1x_1 = (double)(b1_after_1.velocity_x);
        double vel1y_1 = (double)(b1_after_1.velocity_y);
        json(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("vx", (Object)(vel1x_1)), java.util.Map.entry("vy", (Object)(vel1y_1)))));
        Body b3_1 = make_body((double)(-10.0), (double)(0.0), (double)(0.0), (double)(0.0), (double)(1.0));
        Body b4_1 = make_body((double)(10.0), (double)(0.0), (double)(0.0), (double)(0.0), (double)(4.0));
        BodySystem sys2_1 = make_body_system(((Body[])(new Body[]{b3_1, b4_1})), (double)(1.0), (double)(10.0), (double)(0.0));
        sys2_1 = update_system(sys2_1, (double)(1.0));
        Body b2_after_1 = sys2_1.bodies[(int)(0L)];
        double pos2x_1 = (double)(b2_after_1.position_x);
        double pos2y_1 = (double)(b2_after_1.position_y);
        json(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("x", (Object)(pos2x_1)), java.util.Map.entry("y", (Object)(pos2y_1)))));
        double vel2x_1 = (double)(b2_after_1.velocity_x);
        double vel2y_1 = (double)(b2_after_1.velocity_y);
        json(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("vx", (Object)(vel2x_1)), java.util.Map.entry("vy", (Object)(vel2y_1)))));
    }
    public static void main(String[] args) {
        main();
    }

    static void json(Object v) {
        System.out.println(_json(v));
    }

    static String _json(Object v) {
        if (v == null) return "null";
        if (v instanceof String) {
            String s = (String)v;
            s = s.replace("\\", "\\\\").replace("\"", "\\\"");
            return "\"" + s + "\"";
        }
        if (v instanceof Number || v instanceof Boolean) {
            return String.valueOf(v);
        }
        if (v instanceof int[]) {
            int[] a = (int[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof double[]) {
            double[] a = (double[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof boolean[]) {
            boolean[] a = (boolean[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v.getClass().isArray()) {
            Object[] a = (Object[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(_json(a[i])); }
            sb.append("]");
            return sb.toString();
        }
        String s = String.valueOf(v);
        s = s.replace("\\", "\\\\").replace("\"", "\\\"");
        return "\"" + s + "\"";
    }
}
