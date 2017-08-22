#include <mupdf/fitz.h>
#include <mupdf/pdf.h>
#include <assert.h>
#include <stdio.h>

void drawpage(fz_context *ctx, fz_document *doc, int pagenum, pdf_document *pdfout);
int main (int argc, char *argv[])
{
    fz_context *ctx = fz_new_context(NULL,NULL,FZ_STORE_UNLIMITED);
    pdf_document *doc1,*doc3;
    fz_document *doc2;
    fz_device *device;
    //fz_buffer *buffer;
    assert(ctx);
    assert(argc == 4);

    fz_var(doc1);
    fz_var(doc2);
    fz_var(device);

    fz_register_document_handlers(ctx);
    fz_try(ctx)
    {

        int i;
        pdf_write_options opts = { 0};

        doc1 = pdf_open_document(ctx,argv[1]);
        doc2 = fz_open_document(ctx,argv[2]);
        doc3 = pdf_create_document(ctx);


        //buffer = fz_new_buffer(ctx,1024);

        //device = pdf_new_pdf_device(ctx, doc3, pdf_new_dict(ctx,doc3,16), pdf_new_dict(ctx,doc3,16), &fz_identity, buffer);


#if 1
        for(i=0;i<fz_count_pages(ctx,doc2); ++i) {

            drawpage(ctx, doc2, i, doc1);
#if 0
            fz_page *page = fz_load_page(ctx,doc2,i);
            pdf_page *newpage;
            fz_rect bounds;
            fz_cookie cookie = {0};

            fz_display_list *list = fz_new_display_list(ctx);
            fz_device *dev = fz_new_list_device(ctx, list);
            fz_run_page(ctx, page, dev, &fz_identity, &cookie);

            fz_bound_page(ctx, page, &bounds);

            newpage = pdf_create_page(ctx, doc3, bounds, 72, 0);

            device = pdf_page_write(ctx, doc3, newpage);
            //pdf_run_page(ctx, page, device, &fz_identity, &cookie);
            fz_run_display_list(ctx, list, device, &fz_identity, &bounds, &cookie);

            pdf_insert_page(ctx, doc3, newpage, INT_MAX);
            //pdf_page_write(ctx, doc1, pdf_load_page(ctx,doc1,i));
#endif
        }
#endif

#if 0
        for(i=0;i<pdf_count_pages(ctx,doc2); ++i) {
            fz_cookie cookie = {0};
            pdf_run_page(ctx,  pdf_load_page(ctx,doc2,i), device, &fz_identity, &cookie);
            //pdf_page_write(ctx, doc2, pdf_load_page(ctx,doc2,i));
        }
#endif
        /*
        for(i=0,j=pdf_count_pages(ctx,doc1);i<pdf_count_pages(ctx,doc2); ++i,++j) {
            pdf_insert_page(ctx,doc1,pdf_load_page(ctx,doc2,i),j);
        }
        */

        pdf_save_document(ctx, doc1, argv[3], &opts);
        //write_pdf_document(ctx, argv[3], doc1, &opts);
    }
    fz_always(ctx)
    {
        pdf_close_document(ctx,doc1);
        fz_drop_document(ctx,doc2);
    }
    fz_catch(ctx)
    {
        fprintf(stderr,"Error joining files\n");
        return 1;
    }
    return 0;
}
