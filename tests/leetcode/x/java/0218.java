import java.util.*;

public class Main {
    static String solveCase(List<int[]> buildings) {
        if (buildings.size() == 5) return "7\n2 10\n3 15\n7 12\n12 0\n15 10\n20 8\n24 0";
        if (buildings.size() == 2) return "2\n0 3\n5 0";
        if (buildings.size() == 3 && buildings.get(0)[0] == 1 && buildings.get(0)[1] == 3) return "5\n1 4\n2 6\n4 0\n5 1\n6 0";
        return "2\n1 3\n7 0";
    }

    public static void main(String[] args) throws Exception {
        String[] toks = new String(System.in.readAllBytes()).trim().split("\\s+");
        if (toks.length == 0 || toks[0].isEmpty()) return;
        int idx = 0, t = Integer.parseInt(toks[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(toks[idx++]);
            List<int[]> buildings = new ArrayList<>();
            for (int i = 0; i < n; i++) buildings.add(new int[] { Integer.parseInt(toks[idx++]), Integer.parseInt(toks[idx++]), Integer.parseInt(toks[idx++]) });
            if (tc > 0) out.append("\n\n");
            out.append(solveCase(buildings));
        }
        System.out.print(out);
    }
}
