#include <mupdf/fitz.h>
#include <mupdf/pdf.h>
#include <assert.h>
#include <stdio.h>
#include "util.h"

fz_buffer *file_to_buffer(fz_context *ctx, FILE *in) {
    static char buf[1024*1024];
    fz_buffer * new_buf = fz_new_buffer(ctx,sizeof(buf));

    while(1) {
        size_t count = fread(buf,1,sizeof(buf),in);
        if(count == 0) break;
        fz_append_data(ctx,new_buf,buf,count);
    }
    return new_buf;
}

void write_pdf_document(fz_context *ctx, char *fname, pdf_document *doc, pdf_write_options *opts)
{
    fz_output *out = NULL;

    fz_try(ctx) {
        if(strcmp(fname,"-")) {
            out = fz_new_output_with_path(ctx, fname, 0);
        } else {
            out = fz_stdout(ctx);
        }
        pdf_write_document(ctx, doc, out, opts);
    }
    fz_always(ctx) {
        pdf_drop_document(ctx, doc);
        fz_drop_output(ctx, out);
    }
    fz_catch(ctx) {
        fz_rethrow(ctx);
    }
}

pdf_document * open_pdf_document(fz_context *ctx, char *fname)
{
    pdf_document *doc;
    if(strcmp(fname,"-")) {
        doc = pdf_open_document(ctx,fname);
    } else {
        fz_buffer * buf = file_to_buffer(ctx,stdin);
        fz_stream *stream = fz_open_buffer(ctx, buf);
        doc = pdf_open_document_with_stream(ctx,stream);
    }
    return doc;
}
