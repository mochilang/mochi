## Streams and Agents

Define streams and react to emitted events.

```mochi
stream Sensor { id: string, temperature: float }

on Sensor as s {
  print(s.id, s.temperature)
}

emit Sensor { id: "sensor-1", temperature: 22.5 }
```

Agents may store state and expose **intent** functions for external callers.

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
sleep(50)
print(m.status())
```

These features are explained further in the [language specification](../SPEC.md).
