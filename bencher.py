import time as t
import os
import subprocess as sp

problem_dir = "problems"
outfile = "bench-results.txt"
flags = ["", "1", "X", "1X"] 

def sample_file_opt(file, optflags):
    print(".")
    try:
        before = t.time()
        arg_opt = "-o{}".format(optflags)
        full_file = "{}/{}".format(problem_dir, file)
        _ = sp.check_output(["./pingouin", "-dQ", arg_opt, full_file], timeout=35)
        after = t.time()
        return after - before
    except:
        return 35

def measure_file_opt(file, optflags):
    times = [sample_file_opt(file, optflags) for i in range(5)]
    times.sort()
    avg = sum(times) / 5
    print("file [{}], opt [{}]: avg {}".format(file, optflags, avg))
    return avg

def measure_file(file):
    print("Starting measures for [{}]".format(file))
    return { opt:measure_file_opt(file, opt) for opt in flags }

def measure():
    return { file:measure_file(file) for file in os.listdir(problem_dir) }

def main():
    with open(outfile, 'w') as f:
        f.write(str(measure()))

main()
