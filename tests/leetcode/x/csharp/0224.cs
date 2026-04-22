using System;
using System.Collections.Generic;

public class Program
{
    static int Calculate(string expr)
    {
        int result = 0;
        int number = 0;
        int sign = 1;
        var stack = new List<int>();
        foreach (char ch in expr)
        {
            if (char.IsDigit(ch))
            {
                number = number * 10 + (ch - '0');
            }
            else if (ch == '+' || ch == '-')
            {
                result += sign * number;
                number = 0;
                sign = ch == '+' ? 1 : -1;
            }
            else if (ch == '(')
            {
                stack.Add(result);
                stack.Add(sign);
                result = 0;
                number = 0;
                sign = 1;
            }
            else if (ch == ')')
            {
                result += sign * number;
                number = 0;
                int prevSign = stack[stack.Count - 1];
                int prevResult = stack[stack.Count - 2];
                stack.RemoveRange(stack.Count - 2, 2);
                result = prevResult + prevSign * result;
            }
        }
        return result + sign * number;
    }

    public static void Main()
    {
        string first = Console.ReadLine();
        if (first == null) return;
        int t = int.Parse(first.Trim());
        var outLines = new List<string>();
        for (int i = 0; i < t; i++)
        {
            outLines.Add(Calculate(Console.ReadLine() ?? "").ToString());
        }
        Console.Write(string.Join("\n", outLines));
    }
}
