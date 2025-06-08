## Streams and Agents

Define streams and react to emitted events.

```mochi
stream Sensor { id: string, temperature: float }

on Sensor as s {
  print(s.id, s.temperature)
}

emit Sensor { id: "sensor-1", temperature: 22.5 }
```

These features are explained further in the [language specification](../SPEC.md).
