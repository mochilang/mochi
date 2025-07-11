using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.Json;

public record struct Person {
    public string name;
    public int age;
    public string email;
}

class Program {
    static void Main() {
        List<Person> people = _load("../interpreter/valid/people.yaml", new People { format = "yaml" }).Select(e => _cast<Person>(e)).ToList();
        List<Adult> adults = people.Where(p => (p.age >= 18)).Select(p => new Adult { name = p.name, email = p.email }).ToArray();
        foreach (var a in adults) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString(a.name), Convert.ToString(a.email) }));
        }
    }
    public class People {
        public string format;
    }
    
    
    public class Adult {
        public string name;
        public string email;
    }
    
    
    
    static List<dynamic> _load(string path, Dictionary<string, object> opts) {
        var format = opts != null && opts.ContainsKey("format") ? Convert.ToString(opts["format"]) : "csv";
        var header = opts != null && opts.ContainsKey("header") ? Convert.ToBoolean(opts["header"]) : true;
        var delim = opts != null && opts.ContainsKey("delimiter") ? Convert.ToString(opts["delimiter"])[0] : ',';
        string text;
        if (string.IsNullOrEmpty(path) || path == "-") {
            text = Console.In.ReadToEnd();
        } else {
            text = File.ReadAllText(path);
        }
        switch (format) {
        case "jsonl":
            var list = new List<dynamic>();
            foreach (var line in text.Split(new[] { '\n', '\r' }, StringSplitOptions.RemoveEmptyEntries)) list.Add(JsonSerializer.Deserialize<dynamic>(line));
            return list;
        case "json":
            return JsonSerializer.Deserialize<List<dynamic>>(text);
        case "yaml":
            var deser = new DeserializerBuilder().Build();
            var obj = deser.Deserialize<object>(new StringReader(text));
            if (obj is IList<object> lst) return lst.Cast<dynamic>().ToList();
            if (obj is IDictionary<object, object> m) {
                var d = new Dictionary<string, object>();
                foreach (var kv in m) d[Convert.ToString(kv.Key)] = kv.Value;
                return new List<dynamic> { d };
            }
            return new List<dynamic>();
        case "tsv":
            delim = '    '; goto default;
        default:
            var lines = text.Split(new[] { '\n', '\r' }, StringSplitOptions.RemoveEmptyEntries);
            var out = new List<dynamic>();
            string[] headers = null;
            for (int i = 0; i < lines.Length; i++) {
                var parts = lines[i].Split(delim);
                if (i == 0 && header) { headers = parts; continue; }
                var obj = new Dictionary<string, object>();
                for (int j = 0; j < parts.Length; j++) {
                    var key = headers != null && j < headers.Length ? headers[j] : $"col{j}";
                    obj[key] = parts[j];
                }
                out.Add(obj);
            }
            return out;
        }
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
