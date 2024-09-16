#include "mupdf/fitz.h"
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define FAIL -1
#define SUCC 0

typedef struct{
  int x, y;
} offset;

extern fz_context* mupdf_new_context(){
  return fz_new_context(NULL, NULL, FZ_STORE_UNLIMITED);
}

extern int mupdf_register_type_handlers(fz_context* ctx){
  fz_try(ctx)
        fz_register_document_handlers(ctx);
  fz_catch(ctx){
    return FAIL;
  }
  return SUCC;
}

extern void mupdf_drop_context(fz_context* ctx){
  fz_drop_context(ctx);
}

extern fz_document* mupdf_open_document(fz_context* ctx, unsigned char* input){
  fz_document* doc;
  fz_try(ctx)
    doc=fz_open_document(ctx, input);
  fz_catch(ctx){
    return NULL;
  }
  return doc;
}

extern void mupdf_drop_document(fz_context* ctx, fz_document* doc){
  fz_drop_document(ctx, doc);
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
  fz_scale(zm1, zm2);
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

extern void mupdf_drop_pixmap(fz_context* ctx, fz_pixmap* pix){
  fz_drop_pixmap(ctx, pix);
}

extern bool mupdf_pixmap_alphap(fz_pixmap* pix){
  if (pix->alpha)
    return 1;
  else
    return 0;
}

extern offset mupdf_pixmap_offset(fz_pixmap* pix){
  const offset ofs = {pix->x, pix->y};
  return ofs;
}

extern unsigned char* mupdf_extract_bytes_from_pixmap(fz_pixmap* pix){
  int x, y;
  bool alphap = mupdf_pixmap_alphap(pix);
  size_t alpha_index = pix->n - 1;
  int unit_len = alphap ? 4 : 3;
  size_t length = unit_len * pix->w * pix->h;
  unsigned char* s = (unsigned char*)malloc(length);
  for (y = 0; y < pix->h; ++y)
    {
      unsigned char *p = &pix->samples[y * pix->stride];
      for (x = 0; x < pix->w; ++x)
        {
          if (alphap){
              s[0] = p[alpha_index];
              s++;
              memcpy(s, p, 3);
            }
          else{
            memcpy(s, p, 3);
          }
          p += pix->n;
          s += 3;
        }
    }
  s -= length;
  return s;
}
