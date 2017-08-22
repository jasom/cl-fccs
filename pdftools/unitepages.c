#include <mupdf/fitz.h>
#include <mupdf/pdf.h>
#include <assert.h>
#include <stdio.h>
#include "util.h"

/*
 * mudraw -- command line tool for drawing and converting documents
 */

#include "mupdf/fitz.h"
#include "mupdf/pdf.h" /* for pdf output */

#include <sys/time.h>

void drawpage(fz_context *ctx, pdf_document *doc, int pagenum, pdf_document *pdfout)
{
	pdf_page *page;
	fz_display_list *list = NULL;
	fz_device *dev = NULL;
	fz_cookie cookie = { 0 };

	fz_var(list);
	fz_var(dev);


	fz_try(ctx)
		page = pdf_load_page(ctx, doc, pagenum);
	fz_catch(ctx)
		fz_rethrow(ctx);


	{
		fz_matrix ctm = fz_identity;
		fz_rect bounds;

		pdf_bound_page(ctx, page, &bounds);
                pdf_obj *newpage;
                pdf_obj * presources;
                fz_buffer * pcontents;
                    


		fz_try(ctx)
		{
			dev = pdf_page_write(ctx, pdfout, &bounds, &presources, &pcontents);
                        pdf_run_page(ctx, page, dev, &ctm, &cookie);
			fz_drop_device(ctx, dev);
			dev = NULL;
		}
		fz_always(ctx)
		{
			fz_drop_device(ctx, dev);
			dev = NULL;
		}
		fz_catch(ctx)
		{
			fz_drop_display_list(ctx, list);
			fz_rethrow(ctx);
		}
                newpage = pdf_add_page(ctx, pdfout, &bounds, 0, presources, pcontents);
		pdf_insert_page(ctx, pdfout, INT_MAX, newpage);
                /* TODO destroy resources */
	}

	if (list)
		fz_drop_display_list(ctx, list);


	fz_flush_warnings(ctx);

}
int main (int argc, char *argv[])
{
    fz_context *ctx = fz_new_context(NULL,NULL,FZ_STORE_UNLIMITED);
    pdf_document *doc1,*doc2;
    fz_device *device;
    //fz_buffer *buffer;
    assert(ctx);
    assert(argc == 4);

    fz_var(doc1);
    fz_var(doc2);
    fz_var(device);
    fz_try(ctx)
    {
        int i;
        pdf_write_options opts = { 0};

        doc1 = open_pdf_document(ctx,argv[1]);
        doc2 = open_pdf_document(ctx,argv[2]);
        //doc3 = pdf_create_document(ctx);

        //buffer = fz_new_buffer(ctx,1024);

        //device = pdf_new_pdf_device(ctx, doc3, pdf_new_dict(ctx,doc3,16), pdf_new_dict(ctx,doc3,16), &fz_identity, buffer);

#if 1
        for(i=0;i<pdf_count_pages(ctx,doc2); ++i) {
            drawpage(ctx, doc2, i, doc1);
#if 0
            pdf_page *page = pdf_load_page(ctx,doc2,i);
            pdf_page *newpage;
            fz_rect bounds;
            fz_cookie cookie = {0};

            pdf_bound_page(ctx, page, &bounds);

            newpage = pdf_create_page(ctx, doc1, bounds, 72, 0);

            device = pdf_page_write(ctx, doc1, newpage);
            pdf_run_page(ctx, page, device, &fz_identity, &cookie);

            pdf_insert_page(ctx, doc1, newpage, INT_MAX);
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

        write_pdf_document(ctx, argv[3], doc1, &opts);
    }
    fz_always(ctx)
    {
        pdf_drop_document(ctx,doc1);
        pdf_drop_document(ctx,doc2);
    }
    fz_catch(ctx)
    {
        fprintf(stderr,"Error joining files\n");
        return 1;
    }
    return 0;
}
