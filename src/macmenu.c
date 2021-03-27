/* Menu support for GNU Emacs on macOS.
   Copyright (C) 2000-2008  Free Software Foundation, Inc.
   Copyright (C) 2009-2021  YAMAMOTO Mitsuharu

This file is part of GNU Emacs Mac port.

GNU Emacs Mac port is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs Mac port is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs Mac port.  If not, see <https://www.gnu.org/licenses/>.  */

/* Originally contributed by Andrew Choi (akochoi@mac.com) for Emacs 21.  */

#include <config.h>

#include <stdio.h>

#include "lisp.h"
#include "keyboard.h"
#include "frame.h"
#include "termhooks.h"
#include "window.h"
#include "blockinput.h"
#include "buffer.h"
#include "coding.h"

/* This may include sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#include "macterm.h"

/* Load sys/types.h if not already loaded.
   In some systems loading it twice is suicidal.  */
#ifndef makedev
#include <sys/types.h>
#endif

#undef HAVE_MULTILINGUAL_MENU

#include "menu.h"

extern void mac_fill_menubar (widget_value *, bool);
extern int create_and_show_popup_menu (struct frame *, widget_value *,
				       int, int, bool);
extern int create_and_show_dialog (struct frame *, widget_value *);


/* Nonzero means a menu is currently active.  */
static int popup_activated_flag;


/* Set menu_items_inuse so no other popup menu or dialog is created.  */

void
mac_menu_set_in_use (bool in_use)
{
  Lisp_Object frames, frame;

  menu_items_inuse = in_use;
  popup_activated_flag = in_use;

  /* Don't let frames in `above' z-group obscure popups.  */
  FOR_EACH_FRAME (frames, frame)
    {
      struct frame *f = XFRAME (frame);

      if (in_use && FRAME_Z_GROUP_ABOVE (f))
	mac_set_z_group (f, Qabove_suspended, Qabove);
      else if (!in_use && FRAME_Z_GROUP_ABOVE_SUSPENDED (f))
	mac_set_z_group (f, Qabove, Qabove_suspended);
    }
}


DEFUN ("mac-menu-bar-open-internal", Fmac_menu_bar_open_internal,
       Smac_menu_bar_open_internal, 0, 1, "i",
       doc: /* Start key navigation of the menu bar in FRAME.
This initially opens the first menu bar item and you can then navigate with the
arrow keys, select a menu entry with the return key or cancel with the
escape key.  If FRAME has no menu bar this function does nothing.

If FRAME is nil or not given, use the selected frame.  */)
  (Lisp_Object frame)
{
  struct frame *f = decode_window_system_frame (frame);

  mac_activate_menubar (f);

  return Qnil;
}


/* Set the contents of the menubar widgets of frame F.
   The argument FIRST_TIME is currently ignored;
   it is set the first time this is called, from initialize_frame_menubar.  */

void
set_frame_menubar (struct frame *f, bool first_time, bool deep_p)
{
  int menubar_widget = f->output_data.mac->menubar_widget;
  Lisp_Object items;
  widget_value *wv, *first_wv, *prev_wv = 0;
  int i;
  int *submenu_start, *submenu_end;
  int *submenu_top_level_items, *submenu_n_panes;

  eassert (FRAME_MAC_P (f));

  XSETFRAME (Vmenu_updating_frame, f);

  /* This seems to be unnecessary for Carbon.  */
#if 0
  if (! menubar_widget)
    deep_p = true;
#endif

  if (deep_p)
    {
      /* Make a widget-value tree representing the entire menu trees.  */

      struct buffer *prev = current_buffer;
      Lisp_Object buffer;
      ptrdiff_t specpdl_count = SPECPDL_INDEX ();
      int previous_menu_items_used = f->menu_bar_items_used;
      Lisp_Object *previous_items
	= alloca (previous_menu_items_used * sizeof *previous_items);
      int subitems;

      /* If we are making a new widget, its contents are empty,
	 do always reinitialize them.  */
      if (! menubar_widget)
	previous_menu_items_used = 0;

      buffer = XWINDOW (FRAME_SELECTED_WINDOW (f))->contents;
      specbind (Qinhibit_quit, Qt);
      /* Don't let the debugger step into this code
	 because it is not reentrant.  */
      specbind (Qdebug_on_next_call, Qnil);

      record_unwind_save_match_data ();
      if (NILP (Voverriding_local_map_menu_flag))
	{
	  specbind (Qoverriding_terminal_local_map, Qnil);
	  specbind (Qoverriding_local_map, Qnil);
	}

      set_buffer_internal_1 (XBUFFER (buffer));

      /* Run the Lucid hook.  */
      safe_run_hooks (Qactivate_menubar_hook);

      /* If it has changed current-menubar from previous value,
	 really recompute the menubar from the value.  */
      if (! NILP (Vlucid_menu_bar_dirty_flag))
	call0 (Qrecompute_lucid_menubar);
      safe_run_hooks (Qmenu_bar_update_hook);
      fset_menu_bar_items (f, menu_bar_items (FRAME_MENU_BAR_ITEMS (f)));

      items = FRAME_MENU_BAR_ITEMS (f);

      /* Save the frame's previous menu bar contents data.  */
      if (previous_menu_items_used)
	memcpy (previous_items, XVECTOR (f->menu_bar_vector)->contents,
		previous_menu_items_used * word_size);

      /* Fill in menu_items with the current menu bar contents.
	 This can evaluate Lisp code.  */
      save_menu_items ();

      menu_items = f->menu_bar_vector;
      menu_items_allocated = VECTORP (menu_items) ? ASIZE (menu_items) : 0;
      subitems = ASIZE (items) / 4;
      submenu_start = alloca ((subitems + 1) * sizeof *submenu_start);
      submenu_end = alloca (subitems * sizeof *submenu_end);
      submenu_n_panes = alloca (subitems * sizeof *submenu_n_panes);
      submenu_top_level_items = alloca (subitems
					* sizeof *submenu_top_level_items);
      init_menu_items ();
      for (i = 0; i < subitems; i++)
	{
	  Lisp_Object key, string, maps;

	  key = AREF (items, 4 * i);
	  string = AREF (items, 4 * i + 1);
	  maps = AREF (items, 4 * i + 2);
	  if (NILP (string))
	    break;

	  submenu_start[i] = menu_items_used;

	  menu_items_n_panes = 0;
	  submenu_top_level_items[i]
	    = parse_single_submenu (key, string, maps);
	  submenu_n_panes[i] = menu_items_n_panes;

	  submenu_end[i] = menu_items_used;
	}

      submenu_start[i] = -1;
      finish_menu_items ();

      /* Convert menu_items into widget_value trees
	 to display the menu.  This cannot evaluate Lisp code.  */

      wv = make_widget_value ("menubar", NULL, true, Qnil);
      wv->button_type = BUTTON_TYPE_NONE;
      first_wv = wv;

      for (i = 0; submenu_start[i] >= 0; i++)
	{
	  menu_items_n_panes = submenu_n_panes[i];
	  wv = digest_single_submenu (submenu_start[i], submenu_end[i],
				      submenu_top_level_items[i]);
	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    first_wv->contents = wv;
	  /* Don't set wv->name here; GC during the loop might relocate it.  */
	  wv->enabled = true;
	  wv->button_type = BUTTON_TYPE_NONE;
	  prev_wv = wv;
	}

      set_buffer_internal_1 (prev);

      /* If there has been no change in the Lisp-level contents
	 of the menu bar, skip redisplaying it.  Just exit.  */

      /* Compare the new menu items with the ones computed last time.  */
      for (i = 0; i < previous_menu_items_used; i++)
	if (menu_items_used == i
	    || (!EQ (previous_items[i], AREF (menu_items, i))))
	  break;
      if (i == menu_items_used && i == previous_menu_items_used && i != 0)
	{
	  /* The menu items have not changed.  Don't bother updating
	     the menus in any form, since it would be a no-op.  */
	  free_menubar_widget_value_tree (first_wv);
	  discard_menu_items ();
	  unbind_to (specpdl_count, Qnil);
	  return;
	}

      /* The menu items are different, so store them in the frame.  */
      fset_menu_bar_vector (f, menu_items);
      f->menu_bar_items_used = menu_items_used;

      /* This undoes save_menu_items.  */
      unbind_to (specpdl_count, Qnil);

      /* Now GC cannot happen during the lifetime of the widget_value,
	 so it's safe to store data from a Lisp_String.  */
      wv = first_wv->contents;
      for (i = 0; i < ASIZE (items); i += 4)
	{
	  Lisp_Object string;
	  string = AREF (items, i + 1);
	  if (NILP (string))
            break;
          wv->name = SSDATA (string);
          update_submenu_strings (wv->contents);
          wv = wv->next;
	}

    }
  else
    {
      /* Make a widget-value tree containing
	 just the top level menu bar strings.  */

      wv = make_widget_value ("menubar", NULL, true, Qnil);
      wv->button_type = BUTTON_TYPE_NONE;
      first_wv = wv;

      items = FRAME_MENU_BAR_ITEMS (f);
      for (i = 0; i < ASIZE (items); i += 4)
	{
	  Lisp_Object string;

	  string = AREF (items, i + 1);
	  if (NILP (string))
	    break;

	  wv = make_widget_value (SSDATA (string), NULL, true, Qnil);
	  wv->button_type = BUTTON_TYPE_NONE;
	  /* This prevents lwlib from assuming this
	     menu item is really supposed to be empty.  */
	  /* The intptr_t cast avoids a warning.
	     This value just has to be different from small integers.  */
	  wv->call_data = (void *) (intptr_t) (-1);

	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    first_wv->contents = wv;
	  prev_wv = wv;
	}

      /* Forget what we thought we knew about what is in the
	 detailed contents of the menu bar menus.
	 Changing the top level always destroys the contents.  */
      f->menu_bar_items_used = 0;
    }

  /* Create or update the menu bar widget.  */

  block_input ();

  /* Non-null value to indicate menubar has already been "created".  */
  f->output_data.mac->menubar_widget = 1;

  mac_fill_menubar (first_wv->contents, deep_p);

  free_menubar_widget_value_tree (first_wv);

  unblock_input ();
}

/* Get rid of the menu bar of frame F, and free its storage.
   This is used when deleting a frame, and when turning off the menu bar.  */

void
free_frame_menubar (struct frame *f)
{
  f->output_data.mac->menubar_widget = 0;
}


/* Mac_menu_show actually displays a menu using the panes and items in
   menu_items and returns the value selected from it; we assume input
   is blocked by the caller.  */

/* F is the frame the menu is for.
   X and Y are the frame-relative specified position,
   relative to the inside upper left corner of the frame F.
   Bitfield MENUFLAGS bits are:
   MENU_FOR_CLICK is set if this menu was invoked for a mouse click.
   MENU_KEYMAPS is set if this menu was specified with keymaps;
    in that case, we return a list containing the chosen item's value
    and perhaps also the pane's prefix.
   TITLE is the specified menu title.
   ERROR is a place to store an error message string in case of failure.
   (We return nil on failure, but the value doesn't actually matter.)  */

Lisp_Object
mac_menu_show (struct frame *f, int x, int y, int menuflags,
	       Lisp_Object title, const char **error_name)
{
  int i, selection;
  widget_value *wv, *save_wv = 0, *first_wv = 0, *prev_wv = 0;
  widget_value **submenu_stack
    = alloca (menu_items_used * sizeof *submenu_stack);
  Lisp_Object *subprefix_stack
    = alloca (menu_items_used * sizeof *subprefix_stack);
  int submenu_depth = 0;

  eassert (FRAME_MAC_P (f));

  *error_name = NULL;

  if (menu_items_used <= MENU_ITEMS_PANE_LENGTH)
    {
      *error_name = "Empty menu";
      return Qnil;
    }

  if (display_hourglass_p)
    cancel_hourglass ();

  block_input ();

  /* Create a tree of widget_value objects
     representing the panes and their items.  */
  wv = make_widget_value ("menu", NULL, true, Qnil);
  wv->button_type = BUTTON_TYPE_NONE;
  first_wv = wv;
  bool first_pane = true;

  /* Loop over all panes and items, filling in the tree.  */
  i = 0;
  while (i < menu_items_used)
    {
      if (NILP (AREF (menu_items, i)))
	{
	  submenu_stack[submenu_depth++] = save_wv;
	  save_wv = prev_wv;
	  prev_wv = 0;
	  first_pane = true;
	  i++;
	}
      else if (EQ (AREF (menu_items, i), Qlambda))
	{
	  prev_wv = save_wv;
	  save_wv = submenu_stack[--submenu_depth];
	  first_pane = false;
	  i++;
	}
      else if (EQ (AREF (menu_items, i), Qt)
	       && submenu_depth != 0)
	i += MENU_ITEMS_PANE_LENGTH;
      /* Ignore a nil in the item list.
	 It's meaningful only for dialog boxes.  */
      else if (EQ (AREF (menu_items, i), Qquote))
	i += 1;
      else if (EQ (AREF (menu_items, i), Qt))
	{
	  /* Create a new pane.  */
	  Lisp_Object pane_name, prefix;
	  const char *pane_string;

	  pane_name = AREF (menu_items, i + MENU_ITEMS_PANE_NAME);
	  prefix = AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);

#ifndef HAVE_MULTILINGUAL_MENU
	  if (STRINGP (pane_name) && STRING_MULTIBYTE (pane_name))
	    {
	      pane_name = ENCODE_MENU_STRING (pane_name);
	      ASET (menu_items, i + MENU_ITEMS_PANE_NAME, pane_name);
	    }
#endif
	  pane_string = (NILP (pane_name)
			 ? "" : SSDATA (pane_name));
	  /* If there is just one top-level pane, put all its items directly
	     under the top-level menu.  */
	  if (menu_items_n_panes == 1)
	    pane_string = "";

	  /* If the pane has a meaningful name,
	     make the pane a top-level menu item
	     with its items as a submenu beneath it.  */
	  if (!(menuflags & MENU_KEYMAPS) && strcmp (pane_string, ""))
	    {
	      wv = make_widget_value (pane_string, NULL, true, Qnil);
	      if (save_wv)
		save_wv->next = wv;
	      else
		first_wv->contents = wv;
	      if ((menuflags & MENU_KEYMAPS) && !NILP (prefix))
		wv->name++;
	      wv->button_type = BUTTON_TYPE_NONE;
	      save_wv = wv;
	      prev_wv = 0;
	    }
	  else if (first_pane)
	    {
	      save_wv = wv;
	      prev_wv = 0;
	    }
	  first_pane = false;
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  /* Create a new item within current pane.  */
	  Lisp_Object item_name, enable, descrip, def, type, selected, help;
	  item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
	  enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);
	  descrip = AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY);
	  def = AREF (menu_items, i + MENU_ITEMS_ITEM_DEFINITION);
	  type = AREF (menu_items, i + MENU_ITEMS_ITEM_TYPE);
	  selected = AREF (menu_items, i + MENU_ITEMS_ITEM_SELECTED);
	  help = AREF (menu_items, i + MENU_ITEMS_ITEM_HELP);

#ifndef HAVE_MULTILINGUAL_MENU
          if (STRINGP (item_name) && STRING_MULTIBYTE (item_name))
	    {
	      item_name = ENCODE_MENU_STRING (item_name);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_NAME, item_name);
	    }

          if (STRINGP (descrip) && STRING_MULTIBYTE (descrip))
	    {
	      descrip = ENCODE_MENU_STRING (descrip);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY, descrip);
	    }
#endif /* not HAVE_MULTILINGUAL_MENU */

	  wv = make_widget_value (SSDATA (item_name), NULL, !NILP (enable),
				  STRINGP (help) ? help : Qnil);
	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    save_wv->contents = wv;
	  if (!NILP (descrip))
	    wv->key = SSDATA (descrip);
	  /* Use the contents index as call_data, since we are
             restricted to 16-bits.  */
	  wv->call_data = !NILP (def) ? (void *) (intptr_t) i : 0;

	  if (NILP (type))
	    wv->button_type = BUTTON_TYPE_NONE;
	  else if (EQ (type, QCtoggle))
	    wv->button_type = BUTTON_TYPE_TOGGLE;
	  else if (EQ (type, QCradio))
	    wv->button_type = BUTTON_TYPE_RADIO;
	  else
	    emacs_abort ();

	  wv->selected = !NILP (selected);

	  prev_wv = wv;

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  /* Deal with the title, if it is non-nil.  */
  if (!NILP (title))
    {
      widget_value *wv_title;
      widget_value *wv_sep = make_widget_value ("--", NULL, false, Qnil);

      wv_sep->next = first_wv->contents;

#ifndef HAVE_MULTILINGUAL_MENU
      if (STRING_MULTIBYTE (title))
	title = ENCODE_MENU_STRING (title);
#endif

      wv_title = make_widget_value (SSDATA (title), NULL, false, Qnil);
      wv_title->button_type = BUTTON_TYPE_NONE;
      wv_title->next = wv_sep;
      first_wv->contents = wv_title;
    }

  /* Actually create and show the menu until popped down.  */
  selection = create_and_show_popup_menu (f, first_wv, x, y,
					  menuflags & MENU_FOR_CLICK);

  /* Free the widget_value objects we used to specify the contents.  */
  free_menubar_widget_value_tree (first_wv);

  /* Find the selected item, and its pane, to return
     the proper value.  */
  if (selection != 0)
    {
      Lisp_Object prefix, entry;

      prefix = entry = Qnil;
      i = 0;
      while (i < menu_items_used)
	{
	  if (NILP (AREF (menu_items, i)))
	    {
	      subprefix_stack[submenu_depth++] = prefix;
	      prefix = entry;
	      i++;
	    }
	  else if (EQ (AREF (menu_items, i), Qlambda))
	    {
	      prefix = subprefix_stack[--submenu_depth];
	      i++;
	    }
	  else if (EQ (AREF (menu_items, i), Qt))
	    {
	      prefix
		= AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);
	      i += MENU_ITEMS_PANE_LENGTH;
	    }
	  /* Ignore a nil in the item list.
	     It's meaningful only for dialog boxes.  */
	  else if (EQ (AREF (menu_items, i), Qquote))
	    i += 1;
	  else
	    {
	      entry
		= AREF (menu_items, i + MENU_ITEMS_ITEM_VALUE);
	      if (selection == i)
		{
		  if (menuflags & MENU_KEYMAPS)
		    {
		      int j;

		      entry = list1 (entry);
		      if (!NILP (prefix))
			entry = Fcons (prefix, entry);
		      for (j = submenu_depth - 1; j >= 0; j--)
			if (!NILP (subprefix_stack[j]))
			  entry = Fcons (subprefix_stack[j], entry);
		    }
		  unblock_input ();
		  return entry;
		}
	      i += MENU_ITEMS_ITEM_LENGTH;
	    }
	}
    }
  else if (!(menuflags & MENU_FOR_CLICK))
    {
      unblock_input ();
      /* Make "Cancel" equivalent to C-g.  */
      quit ();
    }

  unblock_input ();
  return Qnil;
}


/* Construct native macOS dialog based on widget_value tree.  */

static const char * button_names [] = {
  "button1", "button2", "button3", "button4", "button5",
  "button6", "button7", "button8", "button9", "button10" };

static void
cleanup_widget_value_tree (void *arg)
{
  free_menubar_widget_value_tree (arg);
}

static Lisp_Object
mac_dialog_show (struct frame *f, Lisp_Object title,
		 Lisp_Object header, const char **error_name)
{
  int i, selection, nb_buttons=0;
  char dialog_name[6];

  widget_value *wv, *first_wv = 0, *prev_wv = 0;

  /* Number of elements seen so far, before boundary.  */
  int left_count = 0;
  /* Whether we've seen the boundary between left-hand elts and right-hand.  */
  bool boundary_seen = false;

  ptrdiff_t specpdl_count = SPECPDL_INDEX ();

  eassert (FRAME_MAC_P (f));

  *error_name = NULL;

  if (menu_items_n_panes > 1)
    {
      *error_name = "Multiple panes in dialog box";
      return Qnil;
    }

  /* Create a tree of widget_value objects
     representing the text label and buttons.  */
  {
    Lisp_Object pane_name;
    const char *pane_string;
    pane_name = AREF (menu_items, MENU_ITEMS_PANE_NAME);
    pane_string = (NILP (pane_name)
		   ? "" : SSDATA (pane_name));
    prev_wv = make_widget_value ("message", (char *) pane_string, true, Qnil);
    first_wv = prev_wv;

    /* Loop over all panes and items, filling in the tree.  */
    i = MENU_ITEMS_PANE_LENGTH;
    while (i < menu_items_used)
      {

	/* Create a new item within current pane.  */
	Lisp_Object item_name, enable, descrip;
	item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
	enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);
	descrip
	  = AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY);

	if (NILP (item_name))
	  {
	    free_menubar_widget_value_tree (first_wv);
	    *error_name = "Submenu in dialog items";
	    return Qnil;
	  }
	if (EQ (item_name, Qquote))
	  {
	    /* This is the boundary between left-side elts
	       and right-side elts.  Stop incrementing right_count.  */
	    boundary_seen = true;
	    i++;
	    continue;
	  }
	if (nb_buttons >= 9)
	  {
	    free_menubar_widget_value_tree (first_wv);
	    *error_name = "Too many dialog items";
	    return Qnil;
	  }

	wv = make_widget_value (button_names[nb_buttons],
				SSDATA (item_name),
				!NILP (enable), Qnil);
	prev_wv->next = wv;
	if (!NILP (descrip))
	  wv->key = SSDATA (descrip);
	wv->call_data = (void *) (intptr_t) i;
	  /* menu item is identified by its index in menu_items table */
	prev_wv = wv;

	if (! boundary_seen)
	  left_count++;

	nb_buttons++;
	i += MENU_ITEMS_ITEM_LENGTH;
      }

    /* If the boundary was not specified,
       by default put half on the left and half on the right.  */
    if (! boundary_seen)
      left_count = nb_buttons - nb_buttons / 2;

    wv = make_widget_value (dialog_name, NULL, false, Qnil);

    /*  Frame title: 'Q' = Question, 'I' = Information.
        Can also have 'E' = Error if, one day, we want
        a popup for errors. */
    if (NILP (header))
      dialog_name[0] = 'Q';
    else
      dialog_name[0] = 'I';

    /* Dialog boxes use a really stupid name encoding
       which specifies how many buttons to use
       and how many buttons are on the right. */
    dialog_name[1] = '0' + nb_buttons;
    dialog_name[2] = 'B';
    dialog_name[3] = 'R';
    /* Number of buttons to put on the right.  */
    dialog_name[4] = '0' + nb_buttons - left_count;
    dialog_name[5] = 0;
    wv->contents = first_wv;
    first_wv = wv;
  }

  /* Make sure to free the widget_value objects we used to specify the
     contents even with longjmp.  */
  record_unwind_protect_ptr (cleanup_widget_value_tree, first_wv);

  /* Actually create and show the dialog.  */
  selection = create_and_show_dialog (f, first_wv);

  unbind_to (specpdl_count, Qnil);

  /* Find the selected item, and its pane, to return
     the proper value.  */
  if (selection != 0)
    {
      i = 0;
      while (i < menu_items_used)
	{
	  Lisp_Object entry;

	  if (EQ (AREF (menu_items, i), Qt))
	    i += MENU_ITEMS_PANE_LENGTH;
	  else if (EQ (AREF (menu_items, i), Qquote))
	    {
	      /* This is the boundary between left-side elts and
		 right-side elts.  */
	      ++i;
	    }
	  else
	    {
	      entry
		= AREF (menu_items, i + MENU_ITEMS_ITEM_VALUE);
	      if (selection == i)
		return entry;
	      i += MENU_ITEMS_ITEM_LENGTH;
	    }
	}
    }
  else
    /* Make "Cancel" equivalent to C-g.  */
    quit ();

  return Qnil;
}

Lisp_Object
mac_popup_dialog (struct frame *f, Lisp_Object header, Lisp_Object contents)
{
  Lisp_Object title;
  const char *error_name;
  Lisp_Object selection;
  ptrdiff_t specpdl_count = SPECPDL_INDEX ();

  check_window_system (f);

  /* Decode the dialog items from what was specified.  */
  title = Fcar (contents);
  CHECK_STRING (title);
  record_unwind_protect_void (unuse_menu_items);

  list_of_panes (list1 (contents));

  /* Display them in a dialog box.  */
  block_input ();
  selection = mac_dialog_show (f, title, header, &error_name);
  unblock_input ();

  unbind_to (specpdl_count, Qnil);
  discard_menu_items ();

  if (error_name) error ("%s", error_name);
  return selection;
}



/* Is this item a separator? */
bool
name_is_separator (const char *name)
{
  const char *start = name;

  /* Check if name string consists of only dashes ('-').  */
  while (*name == '-') name++;
  /* Separators can also be of the form "--:TripleSuperMegaEtched"
     or "--deep-shadow".  We don't implement them yet, se we just treat
     them like normal separators.  */
  return (*name == '\0' || start + 2 == name);
}

/* Detect if a menu is currently active.  */

int
popup_activated (void)
{
  return popup_activated_flag;
}

/* The following is used by delayed window autoselection.  */

DEFUN ("menu-or-popup-active-p", Fmenu_or_popup_active_p, Smenu_or_popup_active_p, 0, 0, 0,
       doc: /* Return t if a menu or popup dialog is active.
\(On MS Windows, this refers to the selected frame.)  */)
  (void)
{
  return (popup_activated ()) ? Qt : Qnil;
}

void
syms_of_macmenu (void)
{
  DEFSYM (Qdebug_on_next_call, "debug-on-next-call");
  defsubr (&Smenu_or_popup_active_p);

  defsubr (&Smac_menu_bar_open_internal);
  Ffset (intern_c_string ("accelerate-menu"),
	 intern_c_string (Smac_menu_bar_open_internal.s.symbol_name));

  DEFVAR_LISP ("mac-help-topics", Vmac_help_topics,
    doc: /* List of strings shown as Help topics by Help menu search.
Each element should be a unibyte string in UTF-8.  The special value t
means not to recalculate help topics.  */);
  Vmac_help_topics = Qt;

  DEFVAR_BOOL ("mac-popup-menu-add-contextual-menu",
	       mac_popup_menu_add_contextual_menu,
    doc: /* Non-nil means contextual menu is added to popup menu.  */);
  mac_popup_menu_add_contextual_menu = 0;
}
