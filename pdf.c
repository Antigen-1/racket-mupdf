#include "mupdf/fitz.h"

#define FAIL -1
#define SUCC 0

extern fz_context* mupdf_new_context(int memory_limit){
  return fz_new_context(NULL, NULL, memory_limit);
}

extern int mupdf_register_type_handlers(fz_context* ctx){
  fz_try(ctx)
        fz_register_document_handlers(ctx);
  fz_catch(ctx){
    return FAIL;
  }
  return SUCC;
}

extern int mupdf_drop_context(fz_context* ctx){
  fz_drop_context(ctx);
  return SUCC;
}

extern fz_document* mupdf_open_document(fz_context* ctx, char* input){
  fz_document* doc;
  fz_try(ctx)
    doc=fz_open_document(ctx, input);
  fz_catch(ctx){
    return NULL;
  }
  return doc;
}

extern int mupdf_drop_document(fz_context* ctx, fz_document* doc){
  fz_drop_document(ctx, doc);
  return SUCC;
}

extern int mupdf_count_pages(fz_context* ctx, fz_document* doc){
  int page_count;
  fz_try(ctx)
    page_count = fz_count_pages(ctx, doc);
  fz_catch(ctx)
    {
      return FAIL;
    }
  return page_count;
}

extern fz_matrix mupdf_new_matrix(float zm1, float zm2, float rt){
  fz_matrix ctm;
  fz_scale(zm1 / 100, zm2 / 100);
  fz_pre_rotate(ctm, rt);
  return ctm;
}

extern fz_pixmap* mupdf_new_rgb_pixmap_from_page_number(fz_context* ctx, fz_document* doc, int page_number, fz_matrix ctm){
  fz_pixmap* pix;
  fz_try(ctx)
    pix = fz_new_pixmap_from_page_number(ctx, doc, page_number, ctm, fz_device_rgb(ctx), 0);
  fz_catch(ctx)
    {
      return NULL;
    }
  return pix;
}

extern int mupdf_drop_pixmap(fz_context* ctx, fz_pixmap* pix){
  fz_drop_pixmap(ctx, pix);
  return SUCC;
}
