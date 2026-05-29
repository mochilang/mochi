// Package wrapper synthesises Java JNI source for the Mochi-Kotlin bridge.
package wrapper

// bridgeTemplate is the text/template for MochiBridge.java.
// Template data: BridgeData.
const bridgeTemplate = `package com.mochi.bridge.{{.Artifact}};

import org.graalvm.nativeimage.IsolateThread;
import org.graalvm.nativeimage.c.function.CEntryPoint;

public final class MochiBridge {
{{- range .Functions}}
    @CEntryPoint(name = "{{.CName}}")
    public static {{.ReturnType}} {{.JavaName}}(
            IsolateThread thread{{.ParamList}}) {
        {{.Body}}
    }
{{end}}}
`

// handleRegistryTemplate is the text/template for MochiHandleRegistry.java.
const handleRegistryTemplate = `package com.mochi.bridge.{{.Artifact}};

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

public final class MochiHandleRegistry {
    private static final ConcurrentHashMap<Long, Object> handles = new ConcurrentHashMap<>();
    private static final AtomicLong nextHandle = new AtomicLong(1);

    public static long register(Object obj) {
        if (obj == null) return 0;
        long id = nextHandle.getAndIncrement();
        handles.put(id, obj);
        return id;
    }

    public static Object get(long id) {
        return handles.get(id);
    }

    public static void release(long id) {
        handles.remove(id);
    }
}
`

// jniHelpersTemplate is the text/template for MochiJNI.java.
const jniHelpersTemplate = `package com.mochi.bridge.{{.Artifact}};

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;

public final class MochiJNI {
    private static final Gson GSON = new Gson();

    // String (UTF-8 Mochi) <-> String (Java/Kotlin) -- identity since JVM strings are Unicode
    public static String fromMochiString(String s) { return s; }
    public static String toMochiString(String s) { return s != null ? s : ""; }

    // Collections: serialise to/from JSON string for Phase 05
    public static <T> String listToJson(List<T> list) {
        return GSON.toJson(list);
    }

    public static <T> List<T> jsonToList(String json, Class<T> elementType) {
        Type listType = TypeToken.getParameterized(List.class, elementType).getType();
        return GSON.fromJson(json, listType);
    }

    public static <K, V> String mapToJson(Map<K, V> map) {
        return GSON.toJson(map);
    }

    public static <K, V> Map<K, V> jsonToMap(String json, Class<K> keyType, Class<V> valueType) {
        Type mapType = TypeToken.getParameterized(Map.class, keyType, valueType).getType();
        return GSON.fromJson(json, mapType);
    }
}
`
