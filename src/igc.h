#ifndef EMACS_IGC_H
# define EMACS_IGC_H

# ifdef HAVE_MPS

extern void init_igc (void);
extern void syms_of_igc (void);

struct igc_root_list;
extern struct igc_root_list *igc_mem_add_root (void *start, void *end);
extern void igc_mem_remove_root (struct igc_root_list *r);

struct igc_thread_list;
extern struct igc_thread_list *igc_add_current_thread (void);
extern void igc_remove_thread (struct igc_thread_list *thread);

# endif // HAVE_MPS

#endif // EMACS_IGC_H
