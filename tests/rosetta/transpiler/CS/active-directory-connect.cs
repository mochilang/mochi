// Generated by Mochi 0.10.52 on 2025-07-31 10:06 +0700
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Numerics;
using System.Collections;

class LDAPClient {
    public string Base;
    public string Host;
    public long Port;
    public bool UseSSL;
    public string BindDN;
    public string BindPassword;
    public string UserFilter;
    public string GroupFilter;
    public string[] Attributes;
    public override string ToString() => $"LDAPClient {{Base = \"{Base}\", Host = \"{Host}\", Port = {Port}, UseSSL = {UseSSL}, BindDN = \"{BindDN}\", BindPassword = \"{BindPassword}\", UserFilter = \"{UserFilter}\", GroupFilter = \"{GroupFilter}\", Attributes = {Attributes}}}";
}
class Program {
    static bool seededNow = false;
    static long nowSeed = 0;
    static long _now() {
        if (!seededNow) {
            var s = Environment.GetEnvironmentVariable("MOCHI_NOW_SEED");
            if (long.TryParse(s, out var v)) {
                nowSeed = v;
                seededNow = true;
            }
        }
        if (seededNow) {
            nowSeed = unchecked(nowSeed * 1664525 + 1013904223);
            nowSeed %= 9223372036854775783L;
            if (nowSeed < 0) nowSeed += 9223372036854775783L;
            return nowSeed;
        }
        return DateTime.UtcNow.Ticks / 100;
    }
    static long _mem() {
        return GC.GetTotalMemory(false);
    }
    static string _fmt(object v) {
        if (v is Array a) {
            var parts = new List<string>();
            foreach (var x in a) parts.Add(_fmt(x));
            return "[" + string.Join(" ", parts) + "]";
        }
        if (v is System.Collections.IDictionary d) {
            var keys = new List<string>();
            foreach (var k in d.Keys) keys.Add(k.ToString());
            keys.Sort();
            var parts = new List<string>();
            foreach (var k in keys) parts.Add(k + ":" + _fmt(d[k]));
            return "map[" + string.Join(" ", parts) + "]";
        }
        if (v is System.Collections.IEnumerable e && !(v is string)) {
            var parts = new List<string>();
            foreach (var x in e) parts.Add(_fmt(x));
            return string.Join(" ", parts);
        }
        if (v is bool b) return b ? "1" : "0";
        return Convert.ToString(v);
    }
    static string _fmtTop(object v) {
        if (v is Array a && a.Length > 0 && a.GetValue(0) is Array) {
            var parts = new List<string>();
            foreach (var x in a) parts.Add(_fmt(x));
            return string.Join(" ", parts);
        }
        return _fmt(v);
    }
    static bool connect(LDAPClient client_0) {
        return ((client_0.Host != "") && (client_0.Port > 0));
    }

    static void main() {
        LDAPClient client_1 = new LDAPClient{Base = "dc=example,dc=com", Host = "ldap.example.com", Port = 389, UseSSL = false, BindDN = "uid=readonlyuser,ou=People,dc=example,dc=com", BindPassword = "readonlypassword", UserFilter = "(uid=%s)", GroupFilter = "(memberUid=%s)", Attributes = new string[]{"givenName", "sn", "mail", "uid"}};
        if (connect(client_1)) {
            Console.WriteLine(_fmtTop(("Connected to " + client_1.Host)));
        } else {
            Console.WriteLine(_fmtTop("Failed to connect"));
        };
    }

    static void Main() {
        {
            var __memStart = _mem();
            var __start = _now();
            main();
            var __end = _now();
            var __memEnd = _mem();
            var __dur = (__end - __start);
            var __memDiff = __memEnd - __memStart;
            Console.WriteLine(JsonSerializer.Serialize(new SortedDictionary<string, object>{{"name", "main"}, {"duration_us", __dur}, {"memory_bytes", __memDiff}}, new JsonSerializerOptions{ WriteIndented = true }));
        }
    }
}
