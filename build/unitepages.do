CFLAGS="${CFLAGS} -Wall"
LDFLAGS="${LDFLAGS} -lmupdf"
redo-ifchange ../pdftools/unitepages.c ../pdftools/util.c
gcc -o "$3" ${CFLAGS} ${LDFLAGS} ../pdftools/unitepages.c ../pdftools/util.c
