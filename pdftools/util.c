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
        fz_write_buffer(ctx,new_buf,buf,count);
    }
    return new_buf;
}

void write_pdf_document(fz_context *ctx, char *fname, pdf_document *doc, fz_write_options *opts)
{
    if(strcmp(fname,"-")) {
        pdf_write_document(ctx,doc, fname, opts);
    } else {
        char tmpnam[] ="/tmp/pdfoutXXXXXX";
        static char buf[1024*1024];

        mkstemp(tmpnam);
        fz_try(ctx) {
            FILE *in = fopen(tmpnam,"rb");
            pdf_write_document(ctx, doc, tmpnam, opts);
            while(1) {
                size_t count = fread(buf,1,sizeof(buf),in);
                if(count == 0) break;
                fwrite(buf,1,count,stdout);
            }
            fclose(in);
        }
        fz_always(ctx) {
            unlink(tmpnam);
        }
        fz_catch(ctx) {
            fz_rethrow(ctx);
        }
    }
}

pdf_document * open_pdf_document(fz_context *ctx, char *fname)
{
    pdf_document *doc;
    if(strcmp(fname,"-")) {
        doc = pdf_open_document(ctx,fname);
    } else {
        fz_stream *stream = fz_open_buffer(ctx,file_to_buffer(ctx,stdin));
        doc = pdf_open_document_with_stream(ctx,stream);
    }
    return doc;
}
