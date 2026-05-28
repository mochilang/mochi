// mochiAwaitAll awaits a list of tasks sequentially and returns their results.
public func mochiAwaitAll<T: Sendable>(_ tasks: [Task<T, Never>]) async -> [T] {
    var results: [T] = []
    for task in tasks {
        results.append(await task.value)
    }
    return results
}
