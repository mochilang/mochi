// ReflectTool.java -- invoked as: java -jar reflect-tool.jar <jar-path>
// Outputs JSON to stdout describing the public surface of the target JAR.
//
// Rebuild with:
//   javac -source 11 -target 11 ReflectTool.java
//   jar cfe reflect-tool.jar ReflectTool ReflectTool.class
//
// The tool emits JSON matching the Surface schema (schemaVersion=1).
// It does not use any external dependencies -- only the JDK standard library.
import java.io.*;
import java.lang.reflect.*;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.*;
import java.util.jar.*;

public class ReflectTool {

    public static void main(String[] args) throws Exception {
        if (args.length < 1) {
            System.err.println("usage: reflect-tool.jar <jar-path>");
            System.exit(1);
        }
        File jarFile = new File(args[0]);
        if (!jarFile.exists()) {
            System.err.println("error: JAR not found: " + jarFile);
            System.exit(1);
        }

        List<String> classNames = listClasses(jarFile);
        URLClassLoader loader = new URLClassLoader(new URL[]{jarFile.toURI().toURL()},
                ClassLoader.getSystemClassLoader());

        List<String> classJsons = new ArrayList<>();
        for (String name : classNames) {
            try {
                Class<?> cls = loader.loadClass(name);
                if (!isPublic(cls)) continue;
                classJsons.add(classToJSON(cls));
            } catch (Throwable t) {
                // Skip classes that fail to load.
            }
        }
        loader.close();

        StringBuilder out = new StringBuilder();
        out.append("{\"schemaVersion\":1,\"classes\":[");
        for (int i = 0; i < classJsons.size(); i++) {
            if (i > 0) out.append(",");
            out.append(classJsons.get(i));
        }
        out.append("]}");
        System.out.println(out.toString());
    }

    private static List<String> listClasses(File jarFile) throws IOException {
        List<String> names = new ArrayList<>();
        try (JarFile jar = new JarFile(jarFile)) {
            Enumeration<JarEntry> entries = jar.entries();
            while (entries.hasMoreElements()) {
                JarEntry e = entries.nextElement();
                String n = e.getName();
                if (n.endsWith(".class") && !n.contains("$")) {
                    names.add(n.replace('/', '.').replace(".class", ""));
                }
            }
        }
        return names;
    }

    private static boolean isPublic(Class<?> cls) {
        return Modifier.isPublic(cls.getModifiers());
    }

    private static String classToJSON(Class<?> cls) {
        StringBuilder b = new StringBuilder();
        b.append("{");
        b.append("\"name\":").append(quote(cls.getName())).append(",");
        b.append("\"modifiers\":").append(modifiersJSON(cls.getModifiers())).append(",");
        b.append("\"superclass\":").append(
                cls.getSuperclass() != null ? quote(cls.getSuperclass().getName()) : "null").append(",");
        b.append("\"interfaces\":").append(interfacesJSON(cls.getInterfaces())).append(",");
        b.append("\"methods\":").append(methodsJSON(cls.getMethods())).append(",");
        b.append("\"fields\":").append(fieldsJSON(cls.getFields())).append(",");
        b.append("\"constructors\":").append(constructorsJSON(cls.getConstructors()));
        b.append("}");
        return b.toString();
    }

    private static String methodsJSON(Method[] methods) {
        List<String> parts = new ArrayList<>();
        for (Method m : methods) {
            if (!Modifier.isPublic(m.getModifiers())) continue;
            StringBuilder b = new StringBuilder();
            b.append("{");
            b.append("\"name\":").append(quote(m.getName())).append(",");
            b.append("\"modifiers\":").append(modifiersJSON(m.getModifiers())).append(",");
            b.append("\"returnType\":").append(quote(m.getReturnType().getName())).append(",");
            b.append("\"parameterTypes\":").append(typeNamesJSON(m.getParameterTypes())).append(",");
            b.append("\"parameterNames\":[]").append(",");
            b.append("\"genericReturnType\":").append(quote(m.getGenericReturnType().getTypeName())).append(",");
            b.append("\"genericParameterTypes\":").append(genericTypesJSON(m.getGenericParameterTypes())).append(",");
            b.append("\"exceptionTypes\":").append(typeNamesJSON(m.getExceptionTypes())).append(",");
            b.append("\"isVarargs\":").append(m.isVarArgs()).append(",");
            b.append("\"isDeprecated\":").append(m.isAnnotationPresent(Deprecated.class));
            b.append("}");
            parts.add(b.toString());
        }
        return "[" + String.join(",", parts) + "]";
    }

    private static String fieldsJSON(Field[] fields) {
        List<String> parts = new ArrayList<>();
        for (Field f : fields) {
            if (!Modifier.isPublic(f.getModifiers())) continue;
            StringBuilder b = new StringBuilder();
            b.append("{");
            b.append("\"name\":").append(quote(f.getName())).append(",");
            b.append("\"modifiers\":").append(modifiersJSON(f.getModifiers())).append(",");
            b.append("\"type\":").append(quote(f.getType().getName())).append(",");
            b.append("\"genericType\":").append(quote(f.getGenericType().getTypeName()));
            b.append("}");
            parts.add(b.toString());
        }
        return "[" + String.join(",", parts) + "]";
    }

    private static String constructorsJSON(Constructor<?>[] ctors) {
        List<String> parts = new ArrayList<>();
        for (Constructor<?> c : ctors) {
            if (!Modifier.isPublic(c.getModifiers())) continue;
            StringBuilder b = new StringBuilder();
            b.append("{");
            b.append("\"modifiers\":").append(modifiersJSON(c.getModifiers())).append(",");
            b.append("\"parameterTypes\":").append(typeNamesJSON(c.getParameterTypes())).append(",");
            b.append("\"parameterNames\":[]").append(",");
            b.append("\"isVarargs\":").append(c.isVarArgs());
            b.append("}");
            parts.add(b.toString());
        }
        return "[" + String.join(",", parts) + "]";
    }

    private static String modifiersJSON(int mods) {
        List<String> tokens = new ArrayList<>();
        if (Modifier.isPublic(mods)) tokens.add("\"public\"");
        if (Modifier.isProtected(mods)) tokens.add("\"protected\"");
        if (Modifier.isPrivate(mods)) tokens.add("\"private\"");
        if (Modifier.isAbstract(mods)) tokens.add("\"abstract\"");
        if (Modifier.isStatic(mods)) tokens.add("\"static\"");
        if (Modifier.isFinal(mods)) tokens.add("\"final\"");
        if (Modifier.isSynchronized(mods)) tokens.add("\"synchronized\"");
        if (Modifier.isNative(mods)) tokens.add("\"native\"");
        return "[" + String.join(",", tokens) + "]";
    }

    private static String interfacesJSON(Class<?>[] ifaces) {
        List<String> parts = new ArrayList<>();
        for (Class<?> i : ifaces) parts.add(quote(i.getName()));
        return "[" + String.join(",", parts) + "]";
    }

    private static String typeNamesJSON(Class<?>[] types) {
        List<String> parts = new ArrayList<>();
        for (Class<?> t : types) parts.add(quote(t.getName()));
        return "[" + String.join(",", parts) + "]";
    }

    private static String genericTypesJSON(Type[] types) {
        List<String> parts = new ArrayList<>();
        for (Type t : types) parts.add(quote(t.getTypeName()));
        return "[" + String.join(",", parts) + "]";
    }

    private static String quote(String s) {
        if (s == null) return "null";
        return "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
    }
}
