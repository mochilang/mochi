require 'ostruct'

def _save(rows, path=nil, opts=nil)
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
end

$people = [OpenStruct.new(name: "Alice", age: 30), OpenStruct.new(name: "Bob", age: 25)]
_save($people, "-", (OpenStruct.new(format: "jsonl")).to_h.transform_keys(&:to_s))
