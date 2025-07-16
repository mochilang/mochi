// chat-server.mochi
import java.util.*;

public class ChatServer {
    static List<String> removeName(List<String> names, String name) {
        List<String> out = Arrays.asList();
        for (String n : names) {
            if (!Objects.equals(n, name)) {
                out.add(n);
            }
        }
        return out;
    }
    static void main() {
        List<String> clients = Arrays.asList();
        Object broadcast = msg -> {
            System.out.println(msg);
        };
        Object add = name -> {
            clients.add(name);
            broadcast("+++ \"" + name + "\" connected +++\n");
        };
        Object send = (name, msg) -> {
            broadcast(name + "> " + msg + "\n");
        };
        Object remove = name -> {
            clients = removeName(clients, name);
            broadcast("--- \"" + name + "\" disconnected ---\n");
        };
        add("Alice");
        add("Bob");
        send("Alice", "Hello Bob!");
        send("Bob", "Hi Alice!");
        remove("Bob");
        remove("Alice");
        broadcast("Server stopping!\n");
    }
    static <T> List<T> append(List<T> list, T item) {
        List<T> res = new ArrayList<>(list);
        res.add(item);
        return res;
    }
    public static void main(String[] args) {
    main();
    }
}
