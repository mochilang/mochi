// Generated 2025-08-13 16:13 +0700

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
    mutable _recv_called: int
    mutable _send_called: int
    mutable close_called: int
}
type SocketMock = {
    mutable _bind_called: int
    mutable _listen_called: int
    mutable _accept_called: int
    mutable _shutdown_called: int
    mutable close_called: int
    mutable _conn: ConnMock
}
type FileMock = {
    mutable _read_called: int
    mutable _data: int array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_conn_mock () =
    let mutable __ret : ConnMock = Unchecked.defaultof<ConnMock>
    try
        __ret <- { _recv_called = 0; _send_called = 0; close_called = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
and conn_recv (_conn: ConnMock) (size: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable _conn = _conn
    let mutable size = size
    try
        _conn._recv_called <- (_conn._recv_called) + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and conn_send (_conn: ConnMock) (_data: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable _conn = _conn
    let mutable _data = _data
    try
        _conn._send_called <- (_conn._send_called) + 1
        __ret
    with
        | Return -> __ret
and conn_close (_conn: ConnMock) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable _conn = _conn
    try
        _conn.close_called <- (_conn.close_called) + 1
        __ret
    with
        | Return -> __ret
and make_socket_mock (_conn: ConnMock) =
    let mutable __ret : SocketMock = Unchecked.defaultof<SocketMock>
    let mutable _conn = _conn
    try
        __ret <- { _bind_called = 0; _listen_called = 0; _accept_called = 0; _shutdown_called = 0; close_called = 0; _conn = _conn }
        raise Return
        __ret
    with
        | Return -> __ret
and socket_bind (sock: SocketMock) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable sock = sock
    try
        sock._bind_called <- (sock._bind_called) + 1
        __ret
    with
        | Return -> __ret
and socket_listen (sock: SocketMock) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable sock = sock
    try
        sock._listen_called <- (sock._listen_called) + 1
        __ret
    with
        | Return -> __ret
and socket_accept (sock: SocketMock) =
    let mutable __ret : ConnMock = Unchecked.defaultof<ConnMock>
    let mutable sock = sock
    try
        sock._accept_called <- (sock._accept_called) + 1
        __ret <- sock._conn
        raise Return
        __ret
    with
        | Return -> __ret
and socket_shutdown (sock: SocketMock) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable sock = sock
    try
        sock._shutdown_called <- (sock._shutdown_called) + 1
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
        __ret <- { _read_called = 0; _data = values }
        raise Return
        __ret
    with
        | Return -> __ret
and file_read (f: FileMock) (size: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable f = f
    let mutable size = size
    try
        if (f._read_called) < (Seq.length (f._data)) then
            let value: int = _idx (f._data) (int (f._read_called))
            f._read_called <- (f._read_called) + 1
            __ret <- value
            raise Return
        f._read_called <- (f._read_called) + 1
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
        let _conn: ConnMock = socket_accept (sock)
        let _: int = conn_recv (_conn) (1024)
        let mutable _data: int = file_read (f) (1024)
        while _data <> 0 do
            conn_send (_conn) (_data)
            _data <- file_read (f) (1024)
        conn_close (_conn)
        socket_shutdown (sock)
        socket_close (sock)
        __ret
    with
        | Return -> __ret
and test_send_file_running_as_expected () =
    let mutable __ret : string = Unchecked.defaultof<string>
    try
        let _conn: ConnMock = make_conn_mock()
        let sock: SocketMock = make_socket_mock (_conn)
        let f: FileMock = file_open()
        send_file (sock) (f)
        if (((((((((sock._bind_called) = 1) && ((sock._listen_called) = 1)) && ((sock._accept_called) = 1)) && ((_conn._recv_called) = 1)) && ((f._read_called) >= 1)) && ((_conn._send_called) = 1)) && ((_conn.close_called) = 1)) && ((sock._shutdown_called) = 1)) && ((sock.close_called) = 1) then
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
