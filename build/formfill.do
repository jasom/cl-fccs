CFLAGS="${CFLAGS} -Wall"
LDFLAGS="${LDFLAGS} -lmupdf"
redo-ifchange ../pdftools/formfill.c ../pdftools/util.c
gcc -o "$3" ${CFLAGS} ${LDFLAGS} ../pdftools/formfill.c ../pdftools/util.c
