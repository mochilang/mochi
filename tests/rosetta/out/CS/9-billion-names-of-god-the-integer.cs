// Generated by Mochi compiler v0.10.30 on 2025-07-18T17:21:44Z
using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static List<int> bigTrim(List<int> a)
    {
        int n = a.Count();
        while ((n > 1) && (a[(int)(n - 1)] == 0))
        {
            a = a.GetRange((int)0, ((int)((n - 1)) - (int)(0)));
            n = (n - 1);
        }
        return a;
    }

    static List<int> bigFromInt(int x)
    {
        if (x == 0)
        {
            return new List<int> { 0 };
        }
        List<int> digits = new List<int>();
        int n = x;
        while (n > 0)
        {
            digits = new List<int>(digits) { (n % 10) };
            n = (n / 10);
        }
        return digits;
    }

    static List<int> bigAdd(List<int> a, List<int> b)
    {
        List<int> res = new List<int>();
        int carry = 0;
        int i = 0;
        while (((i < a.Count()) || (i < b.Count())) || (carry > 0))
        {
            int av = 0;
            if (i < a.Count())
            {
                av = a[(int)i];
            }
            int bv = 0;
            if (i < b.Count())
            {
                bv = b[(int)i];
            }
            int s = ((av + bv) + carry);
            res = new List<int>(res) { (s % 10) };
            carry = (s / 10);
            i = (i + 1);
        }
        return bigTrim(res);
    }

    static List<int> bigSub(List<int> a, List<int> b)
    {
        List<int> res = new List<int>();
        int borrow = 0;
        int i = 0;
        while (i < a.Count())
        {
            int av = a[(int)i];
            int bv = 0;
            if (i < b.Count())
            {
                bv = b[(int)i];
            }
            int diff = ((av - bv) - borrow);
            if (diff < 0)
            {
                diff = (diff + 10);
                borrow = 1;
            }
            else
            {
                borrow = 0;
            }
            res = new List<int>(res) { diff };
            i = (i + 1);
        }
        return bigTrim(res);
    }

    static string bigToString(List<int> a)
    {
        string s = "";
        int i = (a.Count() - 1);
        while (i >= 0)
        {
            s = (s + Convert.ToString(a[(int)i]));
            i = (i - 1);
        }
        return s;
    }

    static int minInt(int a, int b)
    {
        if (a < b)
        {
            return a;
        }
        else
        {
            return b;
        }
    }

    static List<List<int>> cumu(int n)
    {
        List<List<List<int>>> cache = new List<List<List<int>>> { new List<List<int>> { bigFromInt(1) } };
        int y = 1;
        while (y <= n)
        {
            List<List<int>> row = new List<List<int>> { bigFromInt(0) };
            int x = 1;
            while (x <= y)
            {
                List<int> val = cache[(int)(y - x)][(int)minInt(x, (y - x))];
                row = new List<List<int>>(row) { bigAdd(row[(int)(row.Count() - 1)], val) };
                x = (x + 1);
            }
            cache = new List<List<List<int>>>(cache) { row };
            y = (y + 1);
        }
        return cache[(int)n];
    }

    static List<string> row(int n)
    {
        List<List<int>> e = cumu(n);
        List<string> _out = new List<string>();
        int i = 0;
        while (i < n)
        {
            List<int> diff = bigSub(e[(int)(i + 1)], e[(int)i]);
            _out = new List<string>(_out) { bigToString(diff) };
            i = (i + 1);
        }
        return _out;
    }

    static void Main()
    {
        Console.WriteLine("rows:");
        int x = 1;
        while (x < 11)
        {
            List<string> r = row(x);
            string line = "";
            int i = 0;
            while (i < r.Count())
            {
                line = ((string.Concat(line, " ") + r[(int)i]) + " ");
                i = (i + 1);
            }
            Console.WriteLine(line);
            x = (x + 1);
        }
        Console.WriteLine("");
        Console.WriteLine("sums:");
        foreach (var num in new List<int> { 23, 123, 1234 })
        {
            List<string> r = cumu(num);
            Console.WriteLine(((Convert.ToString(num) + " ") + bigToString(r[(int)(r.Count() - 1)])));
        }
    }
}
