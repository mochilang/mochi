def removeName(names, name)
	out = []
	names.each do |n|
		if (n != name)
			out = (out + [n])
		end
	end
	return out
end

def main()
	clients = []
	broadcast = ->(msg){
		puts(msg)
	}
	add = ->(name){
		clients = (clients + [name])
		broadcast.call((("+++ \"" + name) + "\" connected +++\n"))
	}
	send = ->(name, msg){
		broadcast.call((((name + "> ") + msg) + "\n"))
	}
	remove = ->(name){
		clients = removeName(clients, name)
		broadcast.call((("--- \"" + name) + "\" disconnected ---\n"))
	}
	add.call("Alice")
	add.call("Bob")
	send.call("Alice", "Hello Bob!")
	send.call("Bob", "Hi Alice!")
	remove.call("Bob")
	remove.call("Alice")
	broadcast.call("Server stopping!\n")
end

main()
