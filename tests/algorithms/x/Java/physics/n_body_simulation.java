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
body.velocity_x = body.velocity_x + force_x * delta_time;
body.velocity_y = body.velocity_y + force_y * delta_time;
        return body;
    }

    static Body update_position(Body body, double delta_time) {
body.position_x = body.position_x + body.velocity_x * delta_time;
body.position_y = body.position_y + body.velocity_y * delta_time;
        return body;
    }

    static BodySystem make_body_system(Body[] bodies, double g, double tf, double sf) {
        return new BodySystem(bodies, g, tf, sf);
    }

    static double sqrtApprox(double x) {
        double guess = x / 2.0;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static BodySystem update_system(BodySystem system, double delta_time) {
        Body[] bodies = ((Body[])(system.bodies));
        int i_1 = 0;
        while (i_1 < bodies.length) {
            Body body1 = bodies[i_1];
            double force_x = 0.0;
            double force_y = 0.0;
            int j = 0;
            while (j < bodies.length) {
                if (i_1 != j) {
                    Body body2 = bodies[j];
                    double dif_x = body2.position_x - body1.position_x;
                    double dif_y = body2.position_y - body1.position_y;
                    double distance_sq = dif_x * dif_x + dif_y * dif_y + system.softening_factor;
                    double distance = sqrtApprox(distance_sq);
                    double denom = distance * distance * distance;
                    force_x = force_x + system.gravitation_constant * body2.mass * dif_x / denom;
                    force_y = force_y + system.gravitation_constant * body2.mass * dif_y / denom;
                }
                j = j + 1;
            }
            body1 = update_velocity(body1, force_x, force_y, delta_time * system.time_factor);
bodies[i_1] = body1;
            i_1 = i_1 + 1;
        }
        i_1 = 0;
        while (i_1 < bodies.length) {
            Body body = bodies[i_1];
            body = update_position(body, delta_time * system.time_factor);
bodies[i_1] = body;
            i_1 = i_1 + 1;
        }
system.bodies = bodies;
        return system;
    }

    static void main() {
        Body b1 = make_body(0.0, 0.0, 0.0, 0.0, 1.0);
        Body b2 = make_body(10.0, 0.0, 0.0, 0.0, 1.0);
        BodySystem sys1 = make_body_system(((Body[])(new Body[]{b1, b2})), 1.0, 1.0, 0.0);
        sys1 = update_system(sys1, 1.0);
        Body b1_after = sys1.bodies[0];
        double pos1x = b1_after.position_x;
        double pos1y = b1_after.position_y;
        json(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("x", (Object)(pos1x)), java.util.Map.entry("y", (Object)(pos1y)))));
        double vel1x = b1_after.velocity_x;
        double vel1y = b1_after.velocity_y;
        json(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("vx", (Object)(vel1x)), java.util.Map.entry("vy", (Object)(vel1y)))));
        Body b3 = make_body(-10.0, 0.0, 0.0, 0.0, 1.0);
        Body b4 = make_body(10.0, 0.0, 0.0, 0.0, 4.0);
        BodySystem sys2 = make_body_system(((Body[])(new Body[]{b3, b4})), 1.0, 10.0, 0.0);
        sys2 = update_system(sys2, 1.0);
        Body b2_after = sys2.bodies[0];
        double pos2x = b2_after.position_x;
        double pos2y = b2_after.position_y;
        json(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("x", (Object)(pos2x)), java.util.Map.entry("y", (Object)(pos2y)))));
        double vel2x = b2_after.velocity_x;
        double vel2y = b2_after.velocity_y;
        json(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("vx", (Object)(vel2x)), java.util.Map.entry("vy", (Object)(vel2y)))));
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
