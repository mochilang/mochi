%% mochi_atoms declares every atom referenced across Mochi runtime
%% modules. Calling all/0 at boot pre-populates the BEAM atom table
%% so no runtime binary_to_atom/1 calls on user data are ever needed
%% (only binary_to_existing_atom/2, which is safe).
-module(mochi_atoms).
-export([all/0]).

all() ->
    [mochi_record_tag, mochi_error, mochi_break, mochi_continue,
     mochi_err_divzero, mochi_err_index, mochi_err_type,
     mochi_err_utf8, mochi_err_async_crash,
     ok, error, some, none, true, false].
