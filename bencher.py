# Usage:
# python3 bencher.py problems/*

import time as t
import os
import subprocess as sp
import sys

outfile = "tex/bench-results.txt"

flags = ['1', 'X', 'T']
flag_combs = [''.join(flags[k] if i & (1<<k) > 0 else '' for k in range(len(flags))) for i in range(2**len(flags))]
print(flag_combs)

def sample_file_opt(file, optflags):
    print(".")
    try:
        before = t.time()
        arg_opt = "-o{}".format(optflags)
        _ = sp.check_output(["./pingouin", "-dQ", arg_opt, file], timeout=35)
        after = t.time()
        result = after - before
        print("\x1b[1A{}".format(result))
        return result
    except:
        return 35

def measure_file_opt(file, optflags):
    times = []
    n = 5
    t = sample_file_opt(file, optflags)
    if t > 30:
        print("Aborted")
        return 35
    elif t < 0.1:
        n = 50
    elif t < 1:
        n = 20
    times.append(t)
    for i in range(n):
        t = sample_file_opt(file, optflags)
        if t > 30:
            print("Aborted")
            return 35
        times.append(t)
    avg = sum(times) / n
    print("file [{}], opt [{}]: avg {}".format(file, optflags, avg))
    return avg

def measure_file(file):
    print("Starting measures for [{}]".format(file))
    return { opt:measure_file_opt(file, opt) for opt in flag_combs }

def measure():
    return { file:measure_file(file) for file in sys.argv[1:] }

def main():
    with open(outfile, 'r') as f:
        text = f.read()
        if text == "":
            data_save = {}
        else:
            data_save = eval(text)
    data = measure()
    for k in data:
        data_save[k] = data[k]
    with open(outfile, 'w') as f:
        f.write(str(data_save))

main()
