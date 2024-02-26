#ifndef EMACS_IGC_H
# define EMACS_IGC_H

# ifdef HAVE_MPS

extern void init_igc (void);
extern void syms_of_igc (void);

extern void *igc_mem_insert (void *start, void *end);
extern void igc_mem_delete (void *info);

extern void *igc_thread_add (const void *cold);
extern void igc_thread_remove (void *info);

# endif // HAVE_MPS

#endif // EMACS_IGC_H
