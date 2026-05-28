// MochiStream and MochiSub: synchronous broadcast stream for Mochi transpiled code.
// Producers emit values; each subscriber receives every value emitted after subscription.

public final class MochiStream<T> {
    private var subscribers: [MochiSub<T>] = []

    public init(capacity: Int64) {}

    public func emit(_ value: T) {
        for sub in subscribers {
            sub.buffer.append(value)
        }
    }

    public func subscribe() -> MochiSub<T> {
        let sub = MochiSub<T>()
        subscribers.append(sub)
        return sub
    }
}

public final class MochiSub<T> {
    var buffer: [T] = []
    private var readIndex: Int = 0

    public init() {}

    public func recv() -> T {
        let v = buffer[readIndex]
        readIndex += 1
        return v
    }
}
