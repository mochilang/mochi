using System;
using System.Collections.Generic;

public class TreeNode
{
    public int val;
    public TreeNode left;
    public TreeNode right;
    public TreeNode(int x) { val = x; }
}

public class Codec
{
    public string serialize(TreeNode root)
    {
        if (root == null) return "[]";
        var outVals = new List<string>();
        var q = new Queue<TreeNode>();
        q.Enqueue(root);
        while (q.Count > 0)
        {
            TreeNode node = q.Dequeue();
            if (node == null)
            {
                outVals.Add("null");
            }
            else
            {
                outVals.Add(node.val.ToString());
                q.Enqueue(node.left);
                q.Enqueue(node.right);
            }
        }
        while (outVals.Count > 0 && outVals[outVals.Count - 1] == "null") outVals.RemoveAt(outVals.Count - 1);
        return "[" + string.Join(",", outVals) + "]";
    }

    public TreeNode deserialize(string data)
    {
        if (data == "[]") return null;
        string[] vals = data.Substring(1, data.Length - 2).Split(',');
        var root = new TreeNode(int.Parse(vals[0]));
        var q = new Queue<TreeNode>();
        q.Enqueue(root);
        int i = 1;
        while (q.Count > 0 && i < vals.Length)
        {
            TreeNode node = q.Dequeue();
            if (i < vals.Length && vals[i] != "null")
            {
                node.left = new TreeNode(int.Parse(vals[i]));
                q.Enqueue(node.left);
            }
            i++;
            if (i < vals.Length && vals[i] != "null")
            {
                node.right = new TreeNode(int.Parse(vals[i]));
                q.Enqueue(node.right);
            }
            i++;
        }
        return root;
    }
}

public class Program
{
    public static void Main()
    {
        string first = Console.ReadLine();
        if (string.IsNullOrEmpty(first)) return;
        int t = int.Parse(first.Trim());
        var codec = new Codec();
        var blocks = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            string line = Console.ReadLine();
            blocks.Add(codec.serialize(codec.deserialize(line.Trim())));
        }
        Console.Write(string.Join("\n\n", blocks));
    }
}
