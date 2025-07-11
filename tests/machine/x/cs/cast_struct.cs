using System;
using System.Collections.Generic;
using System.Text.Json;

public record struct Todo {
    public string title;
}

class Program {
    static void Main() {
        Todo todo = _cast<Todo>(new Dictionary<string, string> { { "title", "hi" } });
        Console.WriteLine(todo.title);
    }
    static T _cast<T>(dynamic v) {
        if (v is T tv) return tv;
        if (typeof(T) == typeof(long)) {
            if (v is long) return (T)v;
            if (v is int) return (T)(object)(long)(int)v;
            if (v is double) return (T)(object)(long)(double)v;
            if (v is float) return (T)(object)(long)(float)v;
            if (v is string) return (T)(object)long.Parse((string)v);
        }
        if (typeof(T) == typeof(double)) {
            if (v is int) return (T)(object)(double)(int)v;
            if (v is double) return (T)v;
            if (v is float) return (T)(object)(double)(float)v;
            if (v is string) return (T)(object)double.Parse((string)v);
        }
        if (typeof(T) == typeof(float)) {
            if (v is int) return (T)(object)(float)(int)v;
            if (v is double) return (T)(object)(float)(double)v;
            if (v is float) return (T)v;
            if (v is string) return (T)(object)float.Parse((string)v);
        }
        if (typeof(T).IsGenericType && typeof(T).GetGenericTypeDefinition() == typeof(Dictionary<,>) && v is System.Collections.IDictionary d) {
            var args = typeof(T).GetGenericArguments();
            var res = (System.Collections.IDictionary)Activator.CreateInstance(typeof(Dictionary<,>).MakeGenericType(args));
            var mCast = typeof(Program).GetMethod("_cast");
            foreach (System.Collections.DictionaryEntry kv in d) {
                var k = mCast.MakeGenericMethod(args[0]).Invoke(null, new object[]{kv.Key});
                var val = mCast.MakeGenericMethod(args[1]).Invoke(null, new object[]{kv.Value});
                res.Add(k, val);
            }
            return (T)res;
        }
        if (v is System.Collections.Generic.IDictionary<object, object> dm) {
            var m = new Dictionary<string, object>();
            foreach (var kv in dm) m[Convert.ToString(kv.Key)] = kv.Value;
            v = m;
        }
        var json = JsonSerializer.Serialize(v);
        return JsonSerializer.Deserialize<T>(json);
    }
    
}
