package dev.mochi.runtime.ai;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.MessageDigest;

public final class AI {

    public static String call(String provider, String prompt) {
        String cassetteDir = System.getenv("MOCHI_LLM_CASSETTE_DIR");
        if (cassetteDir != null) {
            return replayCassette(cassetteDir, provider, prompt);
        }
        throw new dev.mochi.runtime.error.MochiPanicException(99,
            "MOCHI_LLM_CASSETTE_DIR not set; set it to a cassette directory or provide an API key");
    }

    private static String replayCassette(String dir, String provider, String prompt) {
        String key = sha256hex(provider + ":" + prompt);
        var file = Paths.get(dir, key + ".txt");
        try {
            return Files.readString(file, StandardCharsets.UTF_8).stripTrailing();
        } catch (Exception e) {
            throw new dev.mochi.runtime.error.MochiPanicException(99,
                "cassette not found: " + file + " (key=" + key + ")");
        }
    }

    static String sha256hex(String input) {
        try {
            var digest = MessageDigest.getInstance("SHA-256");
            byte[] hash = digest.digest(input.getBytes(StandardCharsets.UTF_8));
            var sb = new StringBuilder(64);
            for (byte b : hash) {
                sb.append(String.format("%02x", b));
            }
            return sb.toString();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
