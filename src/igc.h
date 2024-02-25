#ifndef EMACS_IGC_H
# define EMACS_IGC_H

# ifdef HAVE_MPS

extern void init_igc (void);
extern void syms_of_igc (void);

struct igc_root;
extern struct igc_root *igc_add_mem_root (void *start, void *end);
extern void igc_remove_root (struct igc_root *r);

# endif // HAVE_MPS

#endif // EMACS_IGC_H
