// clang-format off

#ifndef EMACS_IGC_H
#define EMACS_IGC_H

# ifdef HAVE_MPS

/* Assertions.  */
#define IGC_DEBUG 1

/* If defined, allocate conses from MPS.  */
#define IGC_MANAGE_CONS 1
#define IGC_MANAGE_SYMBOLS 1

/* If defined, use a debug AMS pool, and check fenceposts etc.
   See MPS docs.  Can be slow.  */
#define IGC_DEBUG_POOL 1

void igc_break (void);
void init_igc (void);
void syms_of_igc (void);
void *igc_thread_add (const void *cold);
void igc_thread_remove (void *info);
void igc_on_alloc_main_thread_specpdl (void);
void igc_on_grow_specpdl (void);
void igc_on_specbinding_unused (union specbinding *b);
void igc_on_idle (void);
void igc_on_pdump_loaded (void);
void igc_on_make_face_cache (void *face_cache);
void igc_on_free_face_cache (void *face_cache);
void igc_on_face_cache_change (void *face_cache);
void igc_on_adjust_glyph_matrix (void *matrix);
void igc_on_free_glyph_matrix (void *matrix);
void *igc_on_grow_rdstack (void *info, void *start, void *end);

void igc_process_messages (void);
specpdl_ref igc_inhibit_garbage_collection (void);
Lisp_Object igc_make_cons (Lisp_Object car, Lisp_Object cdr);
Lisp_Object igc_alloc_symbol (void);

void * igc_xzalloc (size_t size);
void igc_xfree (void *p);
struct Lisp_Vector *
igc_alloc_pseudovector (size_t memlen, size_t lisplen,
			size_t zerolen, enum pvec_type tag);
struct Lisp_Vector *igc_alloc_vector (ptrdiff_t len);
struct itree_node *igc_make_itree_node (void);

#define eassert_not_mps() eassert(false)
#else
#define igc_break() (void) 0
#define eassert_not_mps() (void) 0
# endif // HAVE_MPS

#endif // EMACS_IGC_H
