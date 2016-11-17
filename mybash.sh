clear
rm exe
/opt/pccts/bin/antlr -gt -gl imperative_parser.g
/opt/pccts/bin/dlg -ci parser.dlg scan.c
g++ -o exe imperative_parser.c scan.c err.c -I/home/soft/PCCTS_v1.33/include -Wno-write-strings
g++ -std=c++11 -o exe imperative_parser.c scan.c err.c -I/home/soft/PCCTS_v1.33/include -Wall -Wno-write-strings -Wextra -Wno-maybe-uninitialized -Wno-unused-parameter -Wno-unused-variable -Wno-empty-body -Wno-sign-compare -Wno-unused-label
rm -f *.o imperative_parser.c scan.c err.c parser.dlg tokens.h mode.h
