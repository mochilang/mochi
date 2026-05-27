package dev.mochi.runtime.io;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;

public final class JSON {

    public static LinkedHashMap<String, String> decode(String json) {
        var result = new LinkedHashMap<String, String>();
        json = json.trim();
        if (!json.startsWith("{")) {
            throw new dev.mochi.runtime.error.MochiPanicException(97,
                "json_decode: expected JSON object");
        }
        // Minimal flat JSON parser — handles string values only.
        json = json.substring(1, json.lastIndexOf('}')).trim();
        if (json.isEmpty()) {
            return result;
        }
        // Split by top-level commas (no nested objects in Phase 14.0).
        var tokens = splitTopLevel(json);
        for (String token : tokens) {
            int colon = token.indexOf(':');
            if (colon < 0) continue;
            String rawKey = token.substring(0, colon).trim();
            String rawVal = token.substring(colon + 1).trim();
            String key = unquote(rawKey);
            String val = unquote(rawVal);
            result.put(key, val);
        }
        return result;
    }

    private static java.util.List<String> splitTopLevel(String s) {
        var parts = new ArrayList<String>();
        int depth = 0;
        var buf = new StringBuilder();
        boolean inStr = false;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == '"' && (i == 0 || s.charAt(i - 1) != '\\')) inStr = !inStr;
            if (!inStr) {
                if (c == '{' || c == '[') depth++;
                else if (c == '}' || c == ']') depth--;
                else if (c == ',' && depth == 0) {
                    parts.add(buf.toString());
                    buf.setLength(0);
                    continue;
                }
            }
            buf.append(c);
        }
        if (buf.length() > 0) parts.add(buf.toString());
        return parts;
    }

    private static String unquote(String s) {
        s = s.trim();
        if (s.startsWith("\"") && s.endsWith("\"")) {
            return s.substring(1, s.length() - 1)
                    .replace("\\\"", "\"")
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\\\", "\\");
        }
        return s;
    }
}
