defmodule Encoder do
  def escape(<<>>), do: ""
  def escape(<<c, rest::binary>>) do
    case c do
      ?" -> "\\\"" <> escape(rest)
      ?\\ -> "\\\\" <> escape(rest)
      ?\n -> "\\n" <> escape(rest)
      ?\r -> "\\r" <> escape(rest)
      ?\t -> "\\t" <> escape(rest)
      _ -> <<c>> <> escape(rest)
    end
  end
  def encode(x) when is_list(x) and not is_bitstring(x), do: "[" <> Enum.map_join(x, ",", &encode/1) <> "]"
  def encode(x) when is_tuple(x), do: encode(Tuple.to_list(x))
  def encode(x) when is_map(x), do: "{" <> Enum.map_join(x, ",", fn {k,v} -> encode(to_string(k)) <> ":" <> encode(v) end) <> "}"
  def encode(x) when is_binary(x), do: "\"" <> escape(x) <> "\""
  def encode(x) when is_atom(x), do: encode(Atom.to_string(x))
  def encode(x) when is_integer(x) or is_float(x), do: to_string(x)
  def encode(true), do: "true"
  def encode(false), do: "false"
  def encode(nil), do: "null"
end

code = IO.read(:stdio, :eof)
{:ok, ast} = Code.string_to_quoted(code, columns: true)
IO.puts(Encoder.encode(ast))
