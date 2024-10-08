# Configuration file for ipython.

c = get_config()

c.InteractiveShellApp.extensions = ["autoreload"]
c.InteractiveShellApp.exec_lines = [
    "%autoreload 2",
    "import sys",
    "import os",
    "import matplotlib.pyplot as plt",
    "import numpy as np",
    "import scipy as sp",
    "import multiprocessing as mp",
    "plt.ion()",
    "plt.show()",
]
