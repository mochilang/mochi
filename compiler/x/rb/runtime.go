//go:build slow

package rbcode

import "sort"

const (
	helperStructify = `def _structify(v)
  case v
  when Hash
    OpenStruct.new(v.transform_keys(&:to_sym).transform_values { |vv| _structify(vv) })
  when Array
    v.map { |vv| _structify(vv) }
  else
    v
  end
end`

	helperFetch = `def _fetch(url, opts=nil)
  require 'net/http'
  require 'json'
  require 'uri'
  uri = URI(url)
  if uri.scheme.nil? || uri.scheme == '' || uri.scheme == 'file'
    path = uri.scheme == 'file' ? uri.path : url
    data = File.read(path)
    return _structify(JSON.parse(data))
  end
  method = 'GET'
  headers = {}
  body = nil
  timeout = nil
  if opts
    method = opts['method'] || method
    if q = opts['query']
      q = URI.encode_www_form(q.to_h)
      uri.query = [uri.query, q].compact.join('&')
    end
    body = JSON.generate(opts['body']) if opts['body']
    if opts['headers']
      opts['headers'].to_h.each { |k,v| headers[k] = v.to_s }
    end
    timeout = opts['timeout']
  end
  req = Net::HTTP.const_get(method.capitalize).new(uri)
  headers.each { |k,v| req[k] = v }
  req.body = body if body
  Net::HTTP.start(uri.host, uri.port, use_ssl: uri.scheme == 'https', read_timeout: timeout) do |http|
    resp = http.request(req)
    return _structify(JSON.parse(resp.body))
  end
end`

	helperLoad = `def _load(path=nil, opts=nil)
  require 'csv'
  require 'json'
  require 'yaml'
  fmt = 'csv'
  header = true
  delim = ','
  if opts
    fmt = opts['format'] || fmt
    header = opts['header'] if opts.key?('header')
    delim = opts['delimiter'] || delim
    delim = delim[0] if delim.is_a?(String) && !delim.empty?
  end
  io = (path.nil? || path == '' || path == '-') ? STDIN : File.open(path, 'r')
  begin
    case fmt
    when 'csv','tsv'
      delim = "\t" if fmt == 'tsv'
      rows = CSV.read(io, col_sep: delim)
      return [] if rows.empty?
      if header
        headers = rows.shift
      else
        m = rows.map(&:length).max || 0
        headers = Array.new(m) { |i| "c#{i}" }
      end
      rows.map do |rec|
        row = {}
        headers.each_with_index do |h,i|
          val = rec[i] || ''
          if val =~ /^-?\d+$/
            row[h] = val.to_i
          elsif val =~ /^-?\d+\.\d+$/
            row[h] = val.to_f
          else
            row[h] = val
          end
        end
        row
      end
    when 'json'
      data = JSON.parse(io.read)
      data.is_a?(Array) ? data.map { |d| d } : [data]
    when 'jsonl'
      io.each_line.map { |l| JSON.parse(l) }
    when 'yaml'
      data = YAML.safe_load(io.read)
      data.is_a?(Array) ? data.map { |d| d } : [data]
    else
      raise "unknown format: #{fmt}"
    end
  ensure
    io.close unless io == STDIN
  end
end`

	helperSave = `def _save(rows, path=nil, opts=nil)
  require 'csv'
  require 'json'
  require 'yaml'
  fmt = 'csv'
  header = false
  delim = ','
  if opts
    fmt = opts['format'] || fmt
    header = opts['header'] if opts.key?('header')
    delim = opts['delimiter'] || delim
    delim = delim[0] if delim.is_a?(String) && !delim.empty?
  end
  rows = rows.map { |r| r.respond_to?(:to_h) ? r.to_h : r }
  io = (path.nil? || path == '' || path == '-') ? STDOUT : File.open(path, 'w')
  begin
    case fmt
    when 'csv','tsv'
      delim = "\t" if fmt == 'tsv'
      csv = CSV.new(io, col_sep: delim)
      headers = rows.empty? ? [] : rows.first.keys.sort
      csv << headers if header
      rows.each do |row|
        csv << headers.map { |h|
          val = row[h]
          if val.is_a?(Hash) || val.is_a?(Array)
            JSON.generate(val)
          elsif val.nil?
            ''
          else
            val.to_s
          end
        }
      end
    when 'json'
      io.write(JSON.generate(rows))
    when 'jsonl'
      rows.each { |row| io.puts(JSON.generate(row)) }
    when 'yaml'
      io.write(YAML.dump(rows.length == 1 ? rows.first : rows))
    else
      raise "unknown format: #{fmt}"
    end
  ensure
    io.close unless io == STDOUT
  end
end`

	helperGenText = `def _gen_text(prompt, model=nil, params=nil)
  prompt
end`

	helperGenEmbed = `def _gen_embed(text, model=nil, params=nil)
  text.chars.map(&:ord).map(&:to_f)
end`

	helperGenStruct = `def _gen_struct(cls, prompt, model=nil, params=nil)
  data = JSON.parse(prompt)
  cls.new(**data.transform_keys(&:to_sym))
end`

	helperJSON = `def _json(v)
  require 'json'
  obj = v
  if v.is_a?(Array)
    obj = v.map { |it| it.respond_to?(:to_h) ? it.to_h : it }
  elsif v.respond_to?(:to_h)
    obj = v.to_h
  end
  puts(JSON.generate(obj))
end`

	helperSum = `def _sum(v)
  list = nil
  if defined?(MGroup) && v.is_a?(MGroup)
    list = v.Items
  elsif v.is_a?(Array)
    list = v
  elsif v.respond_to?(:to_a)
    list = v.to_a
  end
  return 0 if !list || list.empty?
  s = 0.0
  list.each { |n| s += n.to_f }
  s
end`

	helperMin = `def _min(v)
  list = nil
  if v.respond_to?(:Items)
    list = v.Items
  elsif v.is_a?(Array)
    list = v
  elsif v.respond_to?(:to_a)
    list = v.to_a
  end
  return 0 if !list || list.empty?
  list.min
end`

	helperMax = `def _max(v)
  list = nil
  if v.respond_to?(:Items)
    list = v.Items
  elsif v.is_a?(Array)
    list = v
  elsif v.respond_to?(:to_a)
    list = v.to_a
  end
  return 0 if !list || list.empty?
  list.max
end`

	helperFirst = `def _first(v)
  list = nil
  if v.respond_to?(:Items)
    list = v.Items
  elsif v.is_a?(Array)
    list = v
  elsif v.respond_to?(:to_a)
    list = v.to_a
  end
  return nil if !list || list.empty?
  list[0]
end`

	helperEval = `def _eval(code)
  eval(code)
end`

	helperIndexString = `def _indexString(s, i)
  idx = i
  chars = s.chars
  idx += chars.length if idx < 0
  raise 'index out of range' if idx < 0 || idx >= chars.length
  chars[idx]
end`

	helperSliceString = `def _sliceString(s, i, j)
  start = i
  finish = j
  chars = s.chars
  n = chars.length
  start += n if start < 0
  finish += n if finish < 0
  start = 0 if start < 0
  finish = n if finish > n
  finish = start if finish < start
  chars[start...finish].join
end`

	helperReverse = `def _reverse(obj)
  if obj.is_a?(Array)
    obj.reverse
  elsif obj.is_a?(String)
    obj.reverse
  else
    raise 'reverse expects list or string'
  end
end`

	helperSplitString = `def _splitString(s, sep)
  raise 'split expects string' unless s.is_a?(String)
  s.split(sep)
end`

	helperJoinStrings = `def _joinStrings(parts, sep)
  raise 'join expects list' unless parts.is_a?(Array)
  parts.map(&:to_s).join(sep)
end`

	helperGroup = `class MGroup
  include Enumerable
  attr_accessor :key, :Items
  def initialize(k)
    @key = k
    @Items = []
  end
  def length
    @Items.length
  end
  def items
    @Items
  end
  def each(&block)
    @Items.each(&block)
  end
end`

	helperGroupBy = `def _group_by(src, keyfn)
grouped = src.group_by do |it|
  if it.is_a?(Array)
    keyfn.call(*it)
  else
    keyfn.call(it)
  end
end
grouped.map do |k, items|
g = MGroup.new(k)
items.each do |it|
  if it.is_a?(Array) && it.length == 1
    g.Items << it[0]
  else
    g.Items << it
  end
end
g
end
end`

	helperQuery = `def _query(src, joins, opts)
  where_fn = opts['where']
  items = []
  if joins.empty?
    src.each do |v|
      row = [v]
      next if where_fn && !where_fn.call(*row)
      items << row
    end
  else
    items = src.map { |v| [v] }
    joins.each_with_index do |j, idx|
      joined = []
      jitems = j['items']
      on = j['on']
      left = j['left']
      right = j['right']
      last = idx == joins.length - 1
      if right && left
        matched = Array.new(jitems.length, false)
        items.each do |l|
          m = false
          jitems.each_with_index do |r, ri|
            keep = true
            keep = on.call(*l, r) if on
            next unless keep
            m = true
            matched[ri] = true
            row = l + [r]
            if last && where_fn && !where_fn.call(*row)
              next
            end
            joined << row
          end
          row = l + [nil]
          if left && !m
            if last && where_fn && !where_fn.call(*row)
              # skip
            else
              joined << row
            end
          end
        end
        jitems.each_with_index do |r, ri|
          next if matched[ri]
          _undef = Array.new(items[0]&.length || 0, nil)
          row = _undef + [r]
          if last && where_fn && !where_fn.call(*row)
            next
          end
          joined << row
        end
      elsif right
        jitems.each do |r|
          m = false
          items.each do |l|
            keep = true
            keep = on.call(*l, r) if on
            next unless keep
            m = true
            row = l + [r]
            if last && where_fn && !where_fn.call(*row)
              next
            end
            joined << row
          end
          unless m
            _undef = Array.new(items[0]&.length || 0, nil)
            row = _undef + [r]
            if last && where_fn && !where_fn.call(*row)
              next
            end
            joined << row
          end
        end
      else
        items.each do |l|
          m = false
          jitems.each do |r|
            keep = true
            keep = on.call(*l, r) if on
            next unless keep
            m = true
            row = l + [r]
            if last && where_fn && !where_fn.call(*row)
              next
            end
            joined << row
          end
          if left && !m
            row = l + [nil]
            if last && where_fn && !where_fn.call(*row)
              next
            end
            joined << row
          end
        end
      end
      items = joined
    end
  end
  if opts['sortKey']
    items = items.map { |it| [it, opts['sortKey'].call(*it)] }
    items.sort_by! { |p| p[1] }
    items.map!(&:first)
  end
  if opts.key?('skip')
    n = opts['skip']
    items = n < items.length ? items[n..-1] : []
  end
  if opts.key?('take')
    n = opts['take']
    items = n < items.length ? items[0...n] : items
  end
  res = []
  items.each { |r| res << opts['select'].call(*r) }
  res
end`
)

var helperMap = map[string]string{
	"_structify":   helperStructify,
	"_fetch":       helperFetch,
	"_load":        helperLoad,
	"_save":        helperSave,
	"_genText":     helperGenText,
	"_genEmbed":    helperGenEmbed,
	"_genStruct":   helperGenStruct,
	"_json":        helperJSON,
	"_min":         helperMin,
	"_max":         helperMax,
	"_first":       helperFirst,
	"_sum":         helperSum,
	"_eval":        helperEval,
	"_group":       helperGroup,
	"_group_by":    helperGroupBy,
	"_query":       helperQuery,
	"_indexString": helperIndexString,
	"_sliceString": helperSliceString,
	"_reverse":     helperReverse,
	"_splitString": helperSplitString,
	"_joinStrings": helperJoinStrings,
}

func (c *Compiler) use(name string) { c.helpers[name] = true }

func (c *Compiler) emitRuntime() {
	names := make([]string, 0, len(c.helpers))
	for n := range c.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		c.writeln(helperMap[n])
	}
}
