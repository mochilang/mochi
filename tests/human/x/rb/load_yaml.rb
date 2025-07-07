require 'yaml'
path = File.expand_path('../../../interpreter/valid/people.yaml', __dir__)
people = YAML.load_file(path)
adults = people.select { |p| p['age'] >= 18 }
adults.map { |p| { name: p['name'], email: p['email'] } }.each do |a|
  puts "#{a[:name]} #{a[:email]}"
end
