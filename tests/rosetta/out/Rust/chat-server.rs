// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fn main() {
    fn removeName(names: Vec<&'static str>, name: &'static str) -> Vec<&'static str> {
        let mut out: Vec<&'static str> = vec![];
        for n in names {
            if n != name {
                out = { let mut tmp = out.clone(); tmp.push(n); tmp };
            }
        }
        return out;
    }
    fn main() -> () {
        let mut clients: Vec<&'static str> = vec![];
        let broadcast = move |msg: &'static str| -> () {
            println!("{}", vec![format!("{}", msg)].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
        };
        let add = move |name: &'static str| -> () {
            clients = { let mut tmp = clients.clone(); tmp.push(name); tmp };
            broadcast(format!("{}{}", format!("{}{}", "+++ "", name), "" connected +++
"));
        };
        let send = move |name: &'static str, msg: &'static str| -> () {
            broadcast(format!("{}{}", format!("{}{}", format!("{}{}", name, "> "), msg), "
"));
        };
        let remove = move |name: &'static str| -> () {
            clients = removeName(clients, name);
            broadcast(format!("{}{}", format!("{}{}", "--- "", name), "" disconnected ---
"));
        };
        add("Alice");
        add("Bob");
        send("Alice", "Hello Bob!");
        send("Bob", "Hi Alice!");
        remove("Bob");
        remove("Alice");
        broadcast("Server stopping!
");
    }
    main();
}
