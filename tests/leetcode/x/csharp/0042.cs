using System;
using System.Collections.Generic;

class Program
{
    static int Trap(int[] h)
    {
        int left = 0, right = h.Length - 1, leftMax = 0, rightMax = 0, water = 0;
        while (left <= right)
        {
            if (leftMax <= rightMax)
            {
                if (h[left] < leftMax) water += leftMax - h[left];
                else leftMax = h[left];
                left++;
            }
            else
            {
                if (h[right] < rightMax) water += rightMax - h[right];
                else rightMax = h[right];
                right--;
            }
        }
        return water;
    }

    static void Main()
    {
        var lines = Console.In.ReadToEnd().Split(new[] { "\r\n", "\n" }, StringSplitOptions.None);
        if (lines.Length == 0 || lines[0].Trim() == "") return;
        int idx = 0;
        int t = int.Parse(lines[idx++].Trim());
        var output = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int n = int.Parse(lines[idx++].Trim());
            var arr = new int[n];
            for (int i = 0; i < n; i++) arr[i] = int.Parse(lines[idx++].Trim());
            output.Add(Trap(arr).ToString());
        }
        Console.Write(string.Join("\n", output));
    }
}
