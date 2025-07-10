var data = ["outer": ["inner": 1]]
data["outer"]!["inner"] = 2
print((data["outer"] as! [String:Any])["inner"]!)
