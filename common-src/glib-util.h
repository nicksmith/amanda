/*
 * Copyright (c) 2005 Zmanda, Inc.  All Rights Reserved.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License version 2.1 as 
 * published by the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 * 
 * Contact information: Zmanda Inc., 505 N Mathlida Ave, Suite 120
 * Sunnyvale, CA 94085, USA, or: http://www.zmanda.com
 */
/*
 * Utilities that aren't quite included in glib
 *
 * Author: Dustin J. Mitchell <dustin@zmanda.com>, Ian Turner <ian@zmanda.com>
 */

#ifndef GLIB_UTIL_H
#define GLIB_UTIL_H

#include <glib.h>
#include <glib-object.h>

/* like g_[s]list_foreach, but with a function taking only
 * one argument.
 */
#define g_list_foreach_nodata(list, func)				\
    g_list_foreach((list), _glib_util_foreach_glue, (gpointer)(func));
#define g_slist_foreach_nodata(list, func)				\
    g_slist_foreach((list), _glib_util_foreach_glue, (gpointer)(func));
void _glib_util_foreach_glue(gpointer data, gpointer func);

/* This function takes a GValue, which may be zero-filled or
 * initialized. In either case, this function causes the GValue to be
 * initialized with the given type. Note that this function lacks the
 * safety of the standard g_value_ functions; it assumes that the
 * passed value is zeroed or valid.
 *
 * Returns its first argument.*/
GValue* g_value_unset_init(GValue* val, GType type);

/* This does the same thing but also copies the contents of one value
 * into another. Note that this function lacks the safety of the
 * standard g_value_ functions; it assumes that the passed value is
 * zeroed or valid.
 *
 * Returns its second (reset) argument.*/
GValue* g_value_unset_copy(const GValue* from, GValue * to);

/* These functions all take a GLib container, and call free() on all the
 * pointers in the container before free()ing the container itself. */
void g_list_free_full(GList * list);
void g_slist_free_full(GSList * list);
void g_queue_free_full(GQueue * queue);
void g_ptr_array_free_full(GPtrArray * array);

/* g_value_compare() does what you expect. It returns TRUE if and
   only if the two values have the same type and the same value. Note
   that it will return FALSE if the same value is stored with two
   different types: For example, a GValue with a UCHAR of 1 and a
   GValue with a CHAR of 1 will be considered inequal. Also, this is a
   'shallow' comparison; pointers to distinct but equivalent objects
   are considered inequal. */
gboolean g_value_compare(GValue * a, GValue * b);

/* Given a string and a GValue, parse the string and store it in the
   GValue. The GValue should be pre-initalized to whatever type you want
   parsed. */
gboolean g_value_set_from_string(GValue * val, char * string);

/* A GCompareFunc that will sort strings alphabetically (using strcmp) */
gint g_compare_strings(gconstpointer a, gconstpointer b);

#endif
