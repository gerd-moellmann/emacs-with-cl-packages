#ifndef EMACS_IGC_H
# define EMACS_IGC_H

# ifdef HAVE_MPS

extern void init_igc (void);
extern void syms_of_igc (void);

struct igc_root;
extern struct igc_root *igc_mem_add_root (void *start, void *end);
extern void igc_mem_remove_root (struct igc_root *r);

struct igc_thread;
extern struct igc_thread *igc_add_current_thread (void);
extern void igc_remove_thread (struct igc_thread *thread);

# endif // HAVE_MPS

#endif // EMACS_IGC_H
