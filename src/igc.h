#ifndef EMACS_IGC_H
# define EMACS_IGC_H

# ifdef HAVE_MPS

extern void init_igc (void);
extern void syms_of_igc (void);

extern void *igc_mem_insert (void *start, void *end);
extern void igc_mem_delete (void *info);

extern void *igc_thread_add (const void *cold);
extern void igc_thread_remove (void *info);

extern void igc_on_alloc_main_thread_specpdl (void);
extern void igc_on_grow_specpdl (void);
extern void igc_on_specbinding_unused (union specbinding *b);
extern void igc_on_idle (void);

extern void igc_handle_messages (void);

# endif // HAVE_MPS

#endif // EMACS_IGC_H
