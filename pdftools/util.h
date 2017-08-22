#include <mupdf/fitz.h>
#include <mupdf/pdf.h>
#include <assert.h>
#include <stdio.h>
void write_pdf_document(fz_context *ctx, char *fname, pdf_document* doc, pdf_write_options *opts);
pdf_document * open_pdf_document(fz_context *ctx, char *fname);

