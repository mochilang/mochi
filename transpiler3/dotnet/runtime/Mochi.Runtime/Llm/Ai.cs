using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;

namespace Mochi.Runtime.Llm;

public static class Ai
{
    public static string Call(string provider, string prompt)
    {
        string? cassetteDir = Environment.GetEnvironmentVariable("MOCHI_LLM_CASSETTE_DIR");
        if (cassetteDir != null)
            return ReplayCassette(cassetteDir, provider, prompt);

        throw new Exception("MOCHI_LLM_CASSETTE_DIR not set; set it to a cassette directory or provide an API key");
    }

    private static string ReplayCassette(string dir, string provider, string prompt)
    {
        string key = Sha256Hex(provider + ":" + prompt);
        string file = Path.Combine(dir, key + ".txt");
        if (!File.Exists(file))
            throw new Exception($"cassette not found: {file} (key={key})");
        return File.ReadAllText(file, Encoding.UTF8).TrimEnd();
    }

    private static string Sha256Hex(string input)
    {
        byte[] bytes = Encoding.UTF8.GetBytes(input);
        byte[] hash = SHA256.HashData(bytes);
        var sb = new StringBuilder(64);
        foreach (byte b in hash)
            sb.Append(b.ToString("x2"));
        return sb.ToString();
    }
}
