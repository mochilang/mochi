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
  def encode(x) when is_map(x), do: "{" <> Enum.map_join(x, ",", fn {k,v} -> encode(to_string(k)) <> ":" <> encode(v) end) <> "}"
  def encode(x) when is_binary(x), do: "\"" <> escape(x) <> "\""
  def encode(x) when is_atom(x), do: encode(Atom.to_string(x))
  def encode(x) when is_integer(x) or is_float(x), do: to_string(x)
  def encode(true), do: "true"
  def encode(false), do: "false"
  def encode(nil), do: "null"
end

defmodule Builder do
  def stringify(atom) when is_atom(atom), do: Atom.to_string(atom)
  def stringify(term), do: Macro.to_string(term)

  def build({name, meta, args}) when is_list(meta) and is_list(args) do
    %{
      type: stringify(name),
      meta: Map.new(meta, fn {k, v} -> {Atom.to_string(k), build(v)} end),
      args: Enum.map(args, &build/1)
    }
  end

  def build({name, meta, ctx}) when is_list(meta) do
    %{
      type: stringify(name),
      meta: Map.new(meta, fn {k, v} -> {Atom.to_string(k), build(v)} end),
      ctx: build(ctx)
    }
  end

  def build(tuple) when is_tuple(tuple) do
    tuple |> Tuple.to_list() |> Enum.map(&build/1)
  end

  def build(list) when is_list(list), do: Enum.map(list, &build/1)
  def build(atom) when is_atom(atom), do: Atom.to_string(atom)
  def build(other), do: other
end

code = IO.read(:stdio, :eof)
{:ok, ast} = Code.string_to_quoted(code, columns: true)
IO.puts(Encoder.encode(Builder.build(ast)))
