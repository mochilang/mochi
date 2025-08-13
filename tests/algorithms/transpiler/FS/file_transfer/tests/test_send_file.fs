// Generated 2025-08-13 16:01 +0700

exception Return
let mutable _nowSeed:int64 = 0L
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- int64 v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525L + 1013904223L) % 2147483647L
        int _nowSeed
    else
        int (System.DateTime.UtcNow.Ticks % 2147483647L)

_initNow()
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
type ConnMock = {
    mutable recv_called: int
    mutable send_called: int
    mutable close_called: int
}
type SocketMock = {
    mutable bind_called: int
    mutable listen_called: int
    mutable accept_called: int
    mutable shutdown_called: int
    mutable close_called: int
    mutable conn: ConnMock
}
type FileMock = {
    mutable read_called: int
    mutable data: int array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_conn_mock () =
    let mutable __ret : ConnMock = Unchecked.defaultof<ConnMock>
    try
        __ret <- { recv_called = 0; send_called = 0; close_called = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
and conn_recv (conn: ConnMock) (size: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable conn = conn
    let mutable size = size
    try
        conn.recv_called <- (conn.recv_called) + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and conn_send (conn: ConnMock) (data: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable conn = conn
    let mutable data = data
    try
        conn.send_called <- (conn.send_called) + 1
        __ret
    with
        | Return -> __ret
and conn_close (conn: ConnMock) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable conn = conn
    try
        conn.close_called <- (conn.close_called) + 1
        __ret
    with
        | Return -> __ret
and make_socket_mock (conn: ConnMock) =
    let mutable __ret : SocketMock = Unchecked.defaultof<SocketMock>
    let mutable conn = conn
    try
        __ret <- { bind_called = 0; listen_called = 0; accept_called = 0; shutdown_called = 0; close_called = 0; conn = conn }
        raise Return
        __ret
    with
        | Return -> __ret
and socket_bind (sock: SocketMock) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable sock = sock
    try
        sock.bind_called <- (sock.bind_called) + 1
        __ret
    with
        | Return -> __ret
and socket_listen (sock: SocketMock) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable sock = sock
    try
        sock.listen_called <- (sock.listen_called) + 1
        __ret
    with
        | Return -> __ret
and socket_accept (sock: SocketMock) =
    let mutable __ret : ConnMock = Unchecked.defaultof<ConnMock>
    let mutable sock = sock
    try
        sock.accept_called <- (sock.accept_called) + 1
        __ret <- sock.conn
        raise Return
        __ret
    with
        | Return -> __ret
and socket_shutdown (sock: SocketMock) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable sock = sock
    try
        sock.shutdown_called <- (sock.shutdown_called) + 1
        __ret
    with
        | Return -> __ret
and socket_close (sock: SocketMock) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable sock = sock
    try
        sock.close_called <- (sock.close_called) + 1
        __ret
    with
        | Return -> __ret
and make_file_mock (values: int array) =
    let mutable __ret : FileMock = Unchecked.defaultof<FileMock>
    let mutable values = values
    try
        __ret <- { read_called = 0; data = values }
        raise Return
        __ret
    with
        | Return -> __ret
and file_read (f: FileMock) (size: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable f = f
    let mutable size = size
    try
        if (f.read_called) < (Seq.length (f.data)) then
            let value: int = _idx (f.data) (int (f.read_called))
            f.read_called <- (f.read_called) + 1
            __ret <- value
            raise Return
        f.read_called <- (f.read_called) + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and file_open () =
    let mutable __ret : FileMock = Unchecked.defaultof<FileMock>
    try
        __ret <- make_file_mock (unbox<int array> [|1; 0|])
        raise Return
        __ret
    with
        | Return -> __ret
and send_file (sock: SocketMock) (f: FileMock) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable sock = sock
    let mutable f = f
    try
        socket_bind (sock)
        socket_listen (sock)
        let conn: ConnMock = socket_accept (sock)
        let _: int = conn_recv (conn) (1024)
        let mutable data: int = file_read (f) (1024)
        while data <> 0 do
            conn_send (conn) (data)
            data <- file_read (f) (1024)
        conn_close (conn)
        socket_shutdown (sock)
        socket_close (sock)
        __ret
    with
        | Return -> __ret
and test_send_file_running_as_expected () =
    let mutable __ret : string = Unchecked.defaultof<string>
    try
        let conn: ConnMock = make_conn_mock()
        let sock: SocketMock = make_socket_mock (conn)
        let f: FileMock = file_open()
        send_file (sock) (f)
        if (((((((((sock.bind_called) = 1) && ((sock.listen_called) = 1)) && ((sock.accept_called) = 1)) && ((conn.recv_called) = 1)) && ((f.read_called) >= 1)) && ((conn.send_called) = 1)) && ((conn.close_called) = 1)) && ((sock.shutdown_called) = 1)) && ((sock.close_called) = 1) then
            __ret <- "pass"
            raise Return
        __ret <- "fail"
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (test_send_file_running_as_expected()))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
