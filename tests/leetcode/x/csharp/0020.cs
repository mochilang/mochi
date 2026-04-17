using System;
using System.Collections.Generic;

public static class Program
{
    static bool IsValid(string s)
    {
        var stack = new Stack<char>();
        foreach (char ch in s)
        {
            if (ch == '(' || ch == '[' || ch == '{')
            {
                stack.Push(ch);
            }
            else
            {
                if (stack.Count == 0) return false;
                char open = stack.Pop();
                if ((ch == ')' && open != '(') ||
                    (ch == ']' && open != '[') ||
                    (ch == '}' && open != '{')) return false;
            }
        }
        return stack.Count == 0;
    }

    public static void Main()
    {
        string[] parts = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (parts.Length == 0) return;
        int t = int.Parse(parts[0]);
        for (int i = 0; i < t; i++)
        {
            Console.WriteLine(IsValid(parts[i + 1]) ? "true" : "false");
        }
    }
}
