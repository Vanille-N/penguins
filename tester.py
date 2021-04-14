import os
import subprocess as sp
import sys

flags = ['1', 'X', 'T']
flag_combs = [''.join(flags[k] if i & (1<<k) > 0 else '' for k in range(len(flags))) for i in range(2**len(flags))]
print(flag_combs)

class Module:
    def __init__(self, src, **kwargs):
        self.cfg = kwargs
        exec(src, self.cfg)
        self.cfg.pop("__builtins__")

    def get(self, k):
        if k in self.cfg.keys():
            return self.cfg[k]
        else:
            return None

    def __str__(self):
        return "Module({})".format(self.cfg)

def extract_config(file):
    with open(file, 'r') as f:
        text = f.read()
        if "<test>" in text:
            text = text.split("<test>")[1]
            if "</test>" in text:
                text = text.split("</test>")[0]
                try:
                    return Module(text)
                except Exception as e:
                    print("Bad configuration formatting: {}".format(e))
        return Module("pass")

def writer(color=0):
    def write(fmt, *args, **kwargs):
        print("\x1b[{}m".format(color) + fmt.format(*args, **kwargs) + "\x1b[0m")
    return write
success = writer(32) 
failure = writer(31)
warning = writer(33)

path = "problems/"
def verify(file):
    file = path + file
    cfg = extract_config(file)
    expect = cfg.get("expect")
    if expect is None:
        warning("File {} is unchecked".format(file))
        return
    timeout = cfg.get("timeout") or 30
    for f in flag_combs:
        try:
            res = sp.check_output(["./pingouin", "-dQ", "-o"+f, file], timeout=timeout)
            res = int(res)
            if res == expect:
                success("Correct output for {} with -o{}", file, f)
            else:
                failure("{} with -o{} yielded {} instead of {}", file, f, res, expect)
        except sp.TimeoutExpired:
            allow = cfg.get("allow_timeout")
            if allow:
                if type(allow) == list:
                    if f in allow:
                        success("File {} timed out as expected for -o{}", file, f)
                    else:
                        failure("File {} may time out but not for -o{}", file, f)
                else:
                    success("File {} timed out as expected for -o{}", file, f)
            else:
                failure("File {} may not time out for -o{}", file, f)


def main():
    files = sorted(os.listdir(path), reverse=True)
    for f in files:
        verify(f)

main()
