import os
import sys

os.system("./pccts_parser < "+sys.argv[1]+" > programhs.txt ")
os.system("./ejecutable_haskell < input_test")
