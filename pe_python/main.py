#!/usr/bin/env python

import problems_sub_100 as easy_probs
import problems_past_100 as hard_probs

EASYPNUMS = [int(p[1:]) for p in dir(easy_probs) if p[0]=="p"]
HARDPNUMS = [int(p[1:]) for p in dir(hard_probs) if p[0]=="p"]

def execute_problem():
    try:
        n = int(input("Enter problem number: "))
        if n < 1:
            print("Problem number must be >= 1")
        elif n <= 100:
            if n in EASYPNUMS:
                print(getattr(easy_probs, f"p{n:03d}")())
            else:
                print("Problem not solved yet, solved problems are:")
                print(EASYPNUMS)
                pass
        else:
            if n in HARDPNUMS:
                print(getattr(hard_probs, f"p{n:03d}")())
            else:
                print("Problem not solved yet, solved problems are:")
                print(HARDPNUMS)
                pass
            pass
        return True
    except ValueError:
        pass
    return False

if __name__ == '__main__':
    while execute_problem():
        continue
