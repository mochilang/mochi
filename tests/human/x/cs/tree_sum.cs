using System;

abstract class Tree { }

class Leaf : Tree { }

class Node : Tree
{
    public Tree left;
    public int value;
    public Tree right;

    public Node(Tree l, int v, Tree r)
    {
        left = l;
        value = v;
        right = r;
    }
}

class Program
{
    static int SumTree(Tree t) =>
        t switch
        {
            Leaf => 0,
            Node n => SumTree(n.left) + n.value + SumTree(n.right),
            _ => 0,
        };

    static void Main()
    {
        Tree t = new Node(new Leaf(), 1, new Node(new Leaf(), 2, new Leaf()));
        Console.WriteLine(SumTree(t));
    }
}
