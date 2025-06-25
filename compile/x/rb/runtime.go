package rbcode

import "sort"

const (
	helperFetch = `def _fetch(url, opts=nil)
  require 'net/http'
  require 'json'
  require 'uri'
  uri = URI(url)
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
    return JSON.parse(resp.body)
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
  io = (path.nil? || path == '') ? STDIN : File.open(path, 'r')
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
  io = (path.nil? || path == '') ? STDOUT : File.open(path, 'w')
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

	helperGroup = `class MGroup
  attr_accessor :key, :Items
  def initialize(k)
    @key = k
    @Items = []
  end
  def length
    @Items.length
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
g.Items.concat(items)
g
end
end`

	helperQuery = `def _query(src, joins, opts)
  items = src.map { |v| [v] }
  joins.each do |j|
    joined = []
    jitems = j['items']
    on = j['on']
    left = j['left']
    right = j['right']
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
          joined << (l + [r])
        end
        joined << (l + [nil]) unless m
      end
      jitems.each_with_index do |r, ri|
        next if matched[ri]
        undef = Array.new(items[0]&.length || 0, nil)
        joined << (undef + [r])
      end
    elsif right
      jitems.each do |r|
        m = false
        items.each do |l|
          keep = true
          keep = on.call(*l, r) if on
          next unless keep
          m = true
          joined << (l + [r])
        end
        unless m
          undef = Array.new(items[0]&.length || 0, nil)
          joined << (undef + [r])
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
          joined << (l + [r])
        end
        joined << (l + [nil]) if left && !m
      end
    end
    items = joined
  end
  if opts['where']
    items = items.select { |r| opts['where'].call(*r) }
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
	"_fetch":       helperFetch,
	"_load":        helperLoad,
	"_save":        helperSave,
	"_genText":     helperGenText,
	"_genEmbed":    helperGenEmbed,
	"_genStruct":   helperGenStruct,
	"_eval":        helperEval,
	"_group":       helperGroup,
	"_group_by":    helperGroupBy,
	"_query":       helperQuery,
	"_indexString": helperIndexString,
	"_sliceString": helperSliceString,
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
