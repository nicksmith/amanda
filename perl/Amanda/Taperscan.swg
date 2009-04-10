/*
 * Copyright (c) Zmanda, Inc.  All Rights Reserved.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License version 2.1
 * as published by the Free Software Foundation.
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

%module "Amanda::Tapefile"
%include "amglue/amglue.swg"
%include "exception.i"

%{
#include <glib.h>
#include "taperscan.h"
%}


int taper_scan (char* wantlabel,
                char** gotlabel, char** timestamp,
                char **tapedev,
                taper_scan_tracker_t* tracker,
                TaperscanOutputFunctor output_functor,
                void *output_data,
                TaperscanProlongFunctor prolong_functor,
                void *prolong_data
                );

void FILE_taperscan_output_callback(void *data, char *msg);
void CHAR_taperscan_output_callback(void *data, char *msg);

/* Returns a newly allocated tracker object. */
taper_scan_tracker_t* taper_scan_tracker_new(void);

/* Frees a tracker object. */
void taper_scan_tracker_free(taper_scan_tracker_t*);
