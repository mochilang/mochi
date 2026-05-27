using System.Net.Http;

namespace Mochi.Runtime.IO;

public static class Fetch
{
    private static readonly HttpClient _client = new();

    public static string Get(string url) =>
        _client.GetStringAsync(url).GetAwaiter().GetResult().TrimEnd();
}
