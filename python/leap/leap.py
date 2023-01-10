def leap_year(year):
    return by_4(year) and not (by_100(year) and not by_400(year))

def by_4(int):
    if int % 4 == 0:
        return True
    return False

def by_100(int):
    if int % 100 == 0:
        return True
    return False

def by_400(int):
    if int % 400 == 0:
        return True
    return False