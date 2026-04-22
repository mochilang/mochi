import java.io.*;
import java.util.*;

public class Main {
    static class TreeNode {
        int val;
        TreeNode left;
        TreeNode right;
        TreeNode(int x) { val = x; }
    }

    static class Codec {
        String serialize(TreeNode root) {
            if (root == null) return "[]";
            List<String> out = new ArrayList<>();
            LinkedList<TreeNode> q = new LinkedList<>();
            q.add(root);
            while (!q.isEmpty()) {
                TreeNode node = q.poll();
                if (node == null) {
                    out.add("null");
                } else {
                    out.add(Integer.toString(node.val));
                    q.add(node.left);
                    q.add(node.right);
                }
            }
            while (!out.isEmpty() && out.get(out.size() - 1).equals("null")) out.remove(out.size() - 1);
            return "[" + String.join(",", out) + "]";
        }

        TreeNode deserialize(String data) {
            if (data.equals("[]")) return null;
            String[] vals = data.substring(1, data.length() - 1).split(",");
            TreeNode root = new TreeNode(Integer.parseInt(vals[0]));
            LinkedList<TreeNode> q = new LinkedList<>();
            q.add(root);
            int i = 1;
            while (!q.isEmpty() && i < vals.length) {
                TreeNode node = q.poll();
                if (i < vals.length && !vals[i].equals("null")) {
                    node.left = new TreeNode(Integer.parseInt(vals[i]));
                    q.add(node.left);
                }
                i++;
                if (i < vals.length && !vals[i].equals("null")) {
                    node.right = new TreeNode(Integer.parseInt(vals[i]));
                    q.add(node.right);
                }
                i++;
            }
            return root;
        }
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String first = br.readLine();
        if (first == null || first.trim().isEmpty()) return;
        int t = Integer.parseInt(first.trim());
        Codec codec = new Codec();
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            String line = br.readLine();
            if (tc > 0) out.append("\n\n");
            out.append(codec.serialize(codec.deserialize(line.trim())));
        }
        System.out.print(out);
    }
}
