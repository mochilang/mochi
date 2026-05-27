using System.Collections.Generic;
using System.Text.Json;

namespace Mochi.Runtime.IO;

public static class JSON
{
    public static Dictionary<string, string> Decode(string input)
    {
        var result = new Dictionary<string, string>();
        using var doc = JsonDocument.Parse(input);
        foreach (var prop in doc.RootElement.EnumerateObject())
        {
            result[prop.Name] = prop.Value.ValueKind == JsonValueKind.String
                ? prop.Value.GetString() ?? ""
                : prop.Value.ToString();
        }
        return result;
    }
}
