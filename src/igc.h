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
void igc_on_face_cache_change (void *face_cache);
void *igc_on_grow_rdstack (void *info, void *start, void *end);

void igc_process_messages (void);
specpdl_ref igc_inhibit_garbage_collection (void);
Lisp_Object igc_make_cons (Lisp_Object car, Lisp_Object cdr);
Lisp_Object igc_alloc_symbol (void);

void *igc_xzalloc (size_t size);
void *igc_xmalloc (size_t size);
void igc_xfree (void *p);
void *igc_xpalloc (void *pa, ptrdiff_t *nitems, ptrdiff_t nitems_incr_min,
		   ptrdiff_t nitems_max, ptrdiff_t item_size);
void *igc_xnrealloc (void *pa, ptrdiff_t nitems, ptrdiff_t item_size);

struct Lisp_Vector *
igc_alloc_pseudovector (size_t nwords_mem, size_t nwords_lisp,
			size_t nwords_zero, enum pvec_type tag);
struct Lisp_Vector *igc_alloc_vector (ptrdiff_t len);
struct itree_node *igc_make_itree_node (void);
struct image *igc_make_image (void);
struct face *igc_make_face (void);
struct interval *igc_make_interval (void);
struct hash_impl *igc_make_hash_impl (ptrdiff_t nentries);

Lisp_Object igc_make_string (size_t nchars, size_t nbytes, bool unibyte,
			     bool clear);
Lisp_Object igc_make_multibyte_string (size_t nchars, size_t nbytes, bool clear);
Lisp_Object igc_make_unibyte_string (size_t nchars, size_t nbytes, bool clear);

#define eassert_not_mps() eassert(false)
#else
#define igc_break() (void) 0
#define eassert_not_mps() (void) 0
# endif // HAVE_MPS

#endif // EMACS_IGC_H
