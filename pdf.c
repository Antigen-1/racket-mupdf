#include "mupdf/fitz.h"

#define FAIL -1
#define SUCC 0

fz_context* mupdf_new_context(){
  return fz_new_context(NULL, NULL, FZ_STORE_UNLIMITED);
}

int mupdf_register_type_handlers(fz_context* ctx){
  fz_try(ctx)
        fz_register_document_handlers(ctx);
  fz_catch(ctx){
    return FAIL;
  }
  return SUCC;
}

void mupdf_drop_context(fz_context* ctx){
  fz_drop_context(ctx);
}

fz_document* mupdf_open_document(fz_context* ctx, unsigned char* input){
  fz_document* doc = NULL;
  fz_try(ctx)
    doc=fz_open_document(ctx, input);
  fz_catch(ctx){
    return NULL;
  }
  return doc;
}

void mupdf_drop_document(fz_context* ctx, fz_document* doc){
  fz_drop_document(ctx, doc);
}

int mupdf_count_pages(fz_context* ctx, fz_document* doc){
  int page_count;
  fz_try(ctx)
    page_count = fz_count_pages(ctx, doc);
  fz_catch(ctx)
    {
      return FAIL;
    }
  return page_count;
}

fz_matrix mupdf_new_matrix(float zm1, float zm2, float rt){
  fz_matrix ctm = fz_scale(zm1, zm2);
  return fz_pre_rotate(ctm, rt);
}

fz_pixmap* mupdf_new_rgb_pixmap_from_page_number(fz_context* ctx, fz_document* doc, int page_number, fz_matrix ctm){
  fz_pixmap* pix = NULL;
  fz_try(ctx)
    pix = fz_new_pixmap_from_page_number(ctx, doc, page_number, ctm, fz_device_rgb(ctx), 0);
  fz_catch(ctx)
    {
      return NULL;
    }
  return pix;
}

void mupdf_drop_pixmap(fz_context* ctx, fz_pixmap* pix){
  fz_drop_pixmap(ctx, pix);
}

// Pixmap accessors
int mupdf_pixmap_alpha_p(fz_pixmap* pix){
  return pix->alpha;
}
int mupdf_pixmap_w(fz_pixmap* pix){
  return pix->w;
}
int mupdf_pixmap_h(fz_pixmap* pix){
  return pix->h;
}
int mupdf_pixmap_x(fz_pixmap* pix){
  return pix->x;
}
int mupdf_pixmap_y(fz_pixmap* pix){
  return pix->y;
}
int mupdf_pixmap_n(fz_pixmap* pix){
  return pix->n;
}
int mupdf_pixmap_s(fz_pixmap* pix){
  return pix->s;
}
int mupdf_pixmap_stride(fz_pixmap* pix){
  return pix->stride;
}
int mupdf_pixmap_xres(fz_pixmap* pix){
  return pix->xres;
}
int mupdf_pixmap_yres(fz_pixmap* pix){
  return pix->yres;
}
unsigned char* mupdf_pixmap_samples(fz_pixmap* pix){
  return pix->samples;
}
