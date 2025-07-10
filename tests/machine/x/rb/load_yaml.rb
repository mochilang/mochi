require 'ostruct'

def _load(path=nil, opts=nil)
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
end

Person = Struct.new(:name, :age, :email, keyword_init: true)

$people = (_load(File.expand_path("../../../interpreter/valid/people.yaml", __dir__), (OpenStruct.new(format: "yaml")).to_h.transform_keys(&:to_s))).map { |_it| Person.new(**_it) }
$adults = ((($people)).select { |p| (p.age >= 18) }).map { |p| OpenStruct.new(name: p.name, email: p.email) }
$adults.each do |a|
	puts([a.name, a.email].join(" "))
end
