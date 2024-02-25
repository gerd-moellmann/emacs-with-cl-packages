#ifndef EMACS_IGC_H
# define EMACS_IGC_H

# ifdef HAVE_MPS

extern void init_igc (void);
extern void syms_of_igc (void);

struct igc_root;
extern struct igc_root *igc_mem_add_root (void *start, void *end);
extern void igc_mem_remove_root (struct igc_root *r);
extern void igc_register_thread (struct thread_state *state);
extern void igc_deregister_thread (struct thread_state *state);

# endif // HAVE_MPS

#endif // EMACS_IGC_H
