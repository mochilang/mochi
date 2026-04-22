import java.util.*;

public class Main {
    static class Row {
        String dept, name;
        int salary;
        Row(String dept, String name, int salary) { this.dept = dept; this.name = name; this.salary = salary; }
    }

    static String solveCase(Map<Integer, String> deptName, List<String[]> employees) {
        Map<Integer, List<Row>> groups = new HashMap<>();
        for (String[] e : employees) {
            int salary = Integer.parseInt(e[2]);
            int deptId = Integer.parseInt(e[3]);
            groups.computeIfAbsent(deptId, k -> new ArrayList<>()).add(new Row("", e[1], salary));
        }
        List<Row> out = new ArrayList<>();
        for (Map.Entry<Integer, List<Row>> entry : groups.entrySet()) {
            TreeSet<Integer> uniq = new TreeSet<>(Collections.reverseOrder());
            for (Row row : entry.getValue()) uniq.add(row.salary);
            HashSet<Integer> keep = new HashSet<>();
            int i = 0;
            for (int salary : uniq) {
                if (i++ == 3) break;
                keep.add(salary);
            }
            for (Row row : entry.getValue()) if (keep.contains(row.salary)) out.add(new Row(deptName.get(entry.getKey()), row.name, row.salary));
        }
        out.sort((a, b) -> !a.dept.equals(b.dept) ? a.dept.compareTo(b.dept) : a.salary != b.salary ? Integer.compare(b.salary, a.salary) : a.name.compareTo(b.name));
        StringBuilder sb = new StringBuilder();
        sb.append(out.size());
        for (Row row : out) sb.append('\n').append(row.dept).append(',').append(row.name).append(',').append(row.salary);
        return sb.toString();
    }

    public static void main(String[] args) throws Exception {
        String input = new String(System.in.readAllBytes()).trim();
        if (input.isEmpty()) return;
        String[] toks = input.split("\\s+");
        int idx = 0, t = Integer.parseInt(toks[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int d = Integer.parseInt(toks[idx++]), e = Integer.parseInt(toks[idx++]);
            Map<Integer, String> deptName = new HashMap<>();
            for (int i = 0; i < d; i++) deptName.put(Integer.parseInt(toks[idx++]), toks[idx++]);
            List<String[]> employees = new ArrayList<>();
            for (int i = 0; i < e; i++) employees.add(new String[] { toks[idx++], toks[idx++], toks[idx++], toks[idx++] });
            if (tc > 0) out.append("\n\n");
            out.append(solveCase(deptName, employees));
        }
        System.out.print(out);
    }
}
