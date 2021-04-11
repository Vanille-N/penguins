import os

flags = ["", "1", "X", "1X"]

with open("bench-results.txt") as f:
    data = eval(f.read())

def fmt_data(file, flags):
    t = data[file][flags]
    prop = lambda lo, x, hi: int((x - lo) * 100 / (hi - lo))
    if t >= 30:
        return "\\colorbox{black!100}{~\\color{white!100}{$\\infty$}~}"
    elif t >= 10:
        color = "red!100"
        time = "{:.1f}s".format(t)
    elif t >= 5:
        k = prop(5, t, 10)
        color = "red!{}!orange!100".format(k)
        time = "{:.2f}".format(t)
    elif t >= 1:
        k = prop(1, t, 5)
        color = "orange!{}!yellow!100".format(k)
        time = "{:.2f}s".format(t)
    else:
        k = prop(-0.01, t, 1)
        color = "yellow!{}!green!100".format(k)
        time = "{:.0f}ms".format(t * 1000)
    return "\\colorbox{{{}}}{{{}}}".format(color, time)

with open("bench-results.tex", 'w') as f:
    f.write("\\begin{{tabular}}{{|l{}|}}\n".format('|c' * len(flags)))
    f.write("\\hline\n")
    f.write(''.join(" & \\texttt{{-o{}}}".format(f) for f in flags))
    f.write("\\\\\n\\hline\n")
    for file in sorted(os.listdir("problems/")):
        f.write("\\texttt{{{}}}".format(file))
        f.write(''.join(" & \\texttt{{{}}}".format(fmt_data(file, fl)) for fl in flags))
        f.write("\\\\\n")
    f.write("\\hline\n")
    f.write("\\end{tabular}\n")

