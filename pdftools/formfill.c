#include <mupdf/fitz.h>
#include <mupdf/pdf.h>
#include <assert.h>
#include <stdio.h>
#include "util.h"

void fill_a_field(fz_context *ctx, pdf_document *doc, char *field_name, char *value)
{
    pdf_obj *form = pdf_dict_getp(ctx,pdf_trailer(ctx,doc), "Root/AcroForm/Fields");

    pdf_obj *field = pdf_lookup_field(ctx,form, field_name);

    pdf_field_set_value(ctx, doc, field, value);

}


char *read_until_delim(FILE *stream, int delim)
{
    static char buf[1024*1024];
    int i;

    for(i=0;i<sizeof(buf);i++)
    {
        buf[i] = getc(stream);
        if(buf[i] == EOF) {
            return NULL;
        }
        if(buf[i] == delim) {
            break;
        }
    }
    if(i == sizeof(buf)) {
        return NULL;
    }
    buf[i] = 0;

    return strdup(buf);
}

int main (int argc, char *argv[])
{
    fz_context *ctx = fz_new_context(NULL,NULL,FZ_STORE_UNLIMITED);
    pdf_document *doc;
    assert(ctx);
    assert(argc == 4);
    FILE *fields = NULL;

    fz_try(ctx)
    {
        fz_write_options opts = { 0};
        doc = open_pdf_document(ctx,argv[1]);

        fields = fopen(argv[2],"rb");

        if(fields == NULL) {
            fz_throw(ctx,1,"Opening field document failed");
        }

        while(1) {
            char *key = read_until_delim(fields,0);
            char *value = read_until_delim(fields,0);
            if(key && value) {
                fill_a_field(ctx,doc,key,value);
                free(key);
                free(value);
            }
            else {
                break;
            }
        }
        //fill_a_field(ctx,doc,"HeroCharacterName", "Jimmy");
        write_pdf_document(ctx, argv[3], doc, &opts);
    }
    fz_always(ctx)
    {
        pdf_close_document(ctx,doc);
        fclose(fields);
    }
    fz_catch(ctx)
    {
        fprintf(stderr,"Error filling field\n");
        return 1;
    }
    return 0;
}
