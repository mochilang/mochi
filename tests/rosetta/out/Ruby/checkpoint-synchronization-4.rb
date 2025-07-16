$nMech = 5
$detailsPerMech = 4
(1...(($nMech + 1))).each do |mech|
	id = mech
	puts((((("worker " + (id).to_s) + " contracted to assemble ") + ($detailsPerMech).to_s) + " details"))
	puts((("worker " + (id).to_s) + " enters shop"))
	d = 0
	while (d < $detailsPerMech)
		puts((("worker " + (id).to_s) + " assembling"))
		puts((("worker " + (id).to_s) + " completed detail"))
		d = (d + 1)
	end
	puts((("worker " + (id).to_s) + " leaves shop"))
	puts((("mechanism " + (mech).to_s) + " completed"))
end
