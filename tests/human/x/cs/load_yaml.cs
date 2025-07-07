using System;
using System.Collections.Generic;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

class Person
{
    public string name;
    public int age;
    public string email;
}

class Program
{
    static void Main()
    {
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();
        var yaml = File.ReadAllText("../interpreter/valid/people.yaml");
        var people = deserializer.Deserialize<List<Person>>(yaml);
        var adults = people.Where(p => p.age >= 18).Select(p => new { p.name, p.email });
        foreach (var a in adults)
        {
            Console.WriteLine($"{a.name} {a.email}");
        }
    }
}
