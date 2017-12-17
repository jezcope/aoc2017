import ctypes
import sys

lib = ctypes.cdll.LoadLibrary("./spinlock.so")

skip = int(sys.argv[1])

print("Part 1:", lib.spinlock(2017, skip))
print("Part 2:", lib.spinlock0(50_000_000, skip))
