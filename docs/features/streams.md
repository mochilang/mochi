## Streams and Agents

Streams deliver structured events to interested handlers. Use the `stream` keyword to define the fields carried with each event.

```mochi
stream Sensor { id: string, temperature: float }
```

Handlers are declared with `on`. Each time an event is emitted, matching handlers run concurrently.

```mochi
on Sensor as s {
  print(s.id, s.temperature)
}

emit Sensor { id: "sensor-1", temperature: 22.5 }
```

Agents combine handlers with persistent state. They can expose **intent** functions that other code may call or that the MCP server invokes in response to user actions.

```mochi
agent Monitor {
  var count: int = 0

  on Sensor as s {
    count = count + 1
  }

  intent status(): string {
    return "events = " + str(count)
  }
}

let m = Monitor {}
emit Sensor { id: "sensor-2", temperature: 30.0 }
print(m.status())
```

