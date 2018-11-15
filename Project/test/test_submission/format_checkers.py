import re

RADIUS_LIM = 50
MAXINFO_LIM = 20

# check name of command from PEERs
def valid_PEER(name):
    srvs = ['Goloman','Hands','Holiday','Welsh','Wilkes']
    return (name in ['PEER'+srv for srv in srvs])

# check coord format,
# invalid -> False, valid -> (lat, lng)
def valid_coord(coord):
    if not re.fullmatch('[+-]\d*\.?\d+[+-]\d*\.?\d+',coord):
        return False
    else:
        try:
            lat, lng = [ float(num) for num in re.split('[+-]',coord[1:])]
        except ValueError:
            return False
        if not (lat <= 90 and lng <= 180): return False
        sign_lat, sign_lng = (1,1)
        if coord[0] == '-': sign_lat = -1
        if '-' in coord[1:]: sign_lng = -1
        return (sign_lat*lat, sign_lng*lng)

# check if the str is a valid float, between 0 and lim
# invalid -> False, valid -> [float]
def valid_float(s,lim):
    try: f = float(s)
    except ValueError: return False
    if 0 <= f and f <= lim: return [f]
    else: return False
def valid_time(t):
    return valid_float(t, float('inf'))
def valid_radius(r):
    return valid_float(r, RADIUS_LIM)

def valid_int(s,lim):
    try: i = int(s)
    except ValueError: return False
    if 0 <= i and i <= lim: return [i]
    else: return False
def valid_max(s):
    return valid_int(s,MAXINFO_LIM)
