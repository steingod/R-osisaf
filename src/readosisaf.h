/*
 * NAME:
 * readosisaf.h
 *
 * PURPOSE:
 * NA
 *
 * NOTES:
 * NA
 *
 * BUGS:
 * NA
 *
 * AUTHOR:
 * Øystein Godøy, METNO/FOU, 22.09.2006 
 */

#ifndef _READOSISAF_H
#define _READOSISAF_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <safhdf.h>

void readosisafheader(char **infile, char **description, int *year, int
	*month, int *day, int *hour, int *minute, int *xsize, int *ysize,
	int *zsize, double *ucs_ul_x, double *ucs_ul_y, double *ucs_dx,
	double *ucs_dy); 
void readosisafdata(char **infile, double *mymatrix); 
void errmsg(char *where, char *what); 
void logmsg(char *where, char *what); 

#endif /* _READOSISAF_H */
