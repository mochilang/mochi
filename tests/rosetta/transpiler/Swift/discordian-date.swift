// Generated by Mochi transpiler v0.10.41 on 2025-07-26 19:32:52 GMT+7
import Foundation

func _p(_ v: Any?) -> String {
    if let val = v { return String(describing: val) }
    return "<nil>"
}

let dayNames = (["Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle", "Setting Orange"] as! [String])
let seasons = (["Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"] as! [String])
let holydays = ([(["Mungday", "Chaoflux"] as! [String]), (["Mojoday", "Discoflux"] as! [String]), (["Syaday", "Confuflux"] as! [String]), (["Zaraday", "Bureflux"] as! [String]), (["Maladay", "Afflux"] as! [String])] as! [[String]])
func isLeap(_ y: Int) -> Bool {
    if ((y % 400) == 0) {
        return true
    }
    if ((y % 100) == 0) {
        return false
    }
    return Bool(((y % 4) == 0))
}
let daysBefore = ([0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334] as! [Int])
func dayOfYear(_ y: Int, _ m: Int, _ d: Int) -> Int {
    var doy = ((daysBefore[(m - 1)] as! Int) + d)
    if ((m > 2) && Bool(isLeap(Int(y)))) {
        doy = Int((doy + 1))
    }
    return Int(doy)
}
func ordinal(_ n: Int) -> String {
    var suff = "th"
    let mod100 = (n % 100)
    if ((mod100 < 11) || (mod100 > 13)) {
        let r = (n % 10)
        if (r == 1) {
            suff = "st"
        } else if (r == 2) {
            suff = "nd"
        } else if (r == 3) {
            suff = "rd"
        }
        
        
    }
    return String((_p(n) + suff))
}
func discordian(_ y: Int, _ m: Int, _ d: Int) -> String {
    if ((Bool(isLeap(Int(y))) && (m == 2)) && (d == 29)) {
        return String(("St. Tib's Day, YOLD " + _p((y + 1166))))
    }
    var doy = Int(dayOfYear(Int(y), Int(m), Int(d)))
    if (Bool(isLeap(Int(y))) && (doy > 60)) {
        doy = Int((doy - 1))
    }
    var idx = (doy - 1)
    let season = (idx / 73)
    let day = (idx % 73)
    var res = (((((((dayNames[(idx % 5)] as! String) + ", the ") + String(describing: ordinal(Int((day + 1))))) + " day of ") + (seasons[season] as! String)) + " in the YOLD ") + _p((y + 1166)))
    if (day == 4) {
        res = String((((res + ". Celebrate ") + (String(Array(holydays[season])[0]) as! String)) + "!"))
    }
    if (day == 49) {
        res = String((((res + ". Celebrate ") + (String(Array(holydays[season])[1]) as! String)) + "!"))
    }
    return String(res)
}
func main() {
    let dates = ([([2010, 7, 22] as! [Int]), ([2012, 2, 28] as! [Int]), ([2012, 2, 29] as! [Int]), ([2012, 3, 1] as! [Int]), ([2012, 12, 31] as! [Int]), ([2013, 1, 1] as! [Int]), ([2100, 12, 31] as! [Int]), ([2015, 10, 19] as! [Int]), ([2010, 1, 5] as! [Int]), ([2011, 5, 3] as! [Int]), ([2000, 3, 13] as! [Int])] as! [[Int]])
    var i = 0
    while (i < Int(((dates).count))) {
        let dt = (dates[i] as! [Int])
        print(_p(String(describing: discordian((dt[0] as! Int), (dt[1] as! Int), (dt[2] as! Int)))))
        i = Int((i + 1))
    }
}
_ = main()
