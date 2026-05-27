package dev.mochi.runtime.io;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public final class Fetch {
    private static final HttpClient CLIENT = HttpClient.newBuilder()
        .version(HttpClient.Version.HTTP_1_1)
        .followRedirects(HttpClient.Redirect.NORMAL)
        .build();

    public static String get(String url) {
        try {
            var request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .GET()
                .build();
            var response = CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() >= 400) {
                throw new dev.mochi.runtime.error.MochiPanicException(98,
                    "fetch failed: HTTP " + response.statusCode() + " from " + url);
            }
            return response.body();
        } catch (dev.mochi.runtime.error.MochiPanicException e) {
            throw e;
        } catch (Exception e) {
            throw new dev.mochi.runtime.error.MochiPanicException(98,
                "fetch failed: " + e.getMessage());
        }
    }
}
