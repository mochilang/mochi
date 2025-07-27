// Generated 2025-07-27 22:40 +0700

exception Return

type Box = {
    Contents: string
    secret: int
}
let rec Box_TellSecret (self: Box) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable self = self
    try
        __ret <- self.secret
        raise Return
        __ret
    with
        | Return -> __ret
and New () =
    let mutable __ret : Box = Unchecked.defaultof<Box>
    try
        let mutable b: Box = { Contents = "rabbit"; secret = 1 }
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
let mutable box: Box = New()
Box_TellSecret box
