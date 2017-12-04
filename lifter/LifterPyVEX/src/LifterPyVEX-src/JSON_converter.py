import json

def to_JSON(ty, sty, args):
    return {'Type' : ty, 'SubType' : sty, 'Args' : args}

def JSONize(target):
    return json.dumps(target)
