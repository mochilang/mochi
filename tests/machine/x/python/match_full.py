def classify(n):

    def _match0(_t0):
        match _t0:
            case 0:
                return "zero"
            case 1:
                return "one"
            case _:
                return "many"

    return _match0(n)


x = 2


def _match1(_t1):
    match _t1:
        case 1:
            return "one"
        case 2:
            return "two"
        case 3:
            return "three"
        case _:
            return "unknown"


label = _match1(x)
print(label)
day = "sun"


def _match2(_t2):
    match _t2:
        case "mon":
            return "tired"
        case "fri":
            return "excited"
        case "sun":
            return "relaxed"
        case _:
            return "normal"


mood = _match2(day)
print(mood)
ok = True


def _match3(_t3):
    match _t3:
        case True:
            return "confirmed"
        case False:
            return "denied"
        case _:
            return None


status = _match3(ok)
print(status)
print(classify(0))
print(classify(5))
