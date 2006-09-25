/*
 * NAME:
 * readosisaf.c
 *
 * PURPOSE:
 * To read OSISAF HDF5 files.
 *
 * NOTES:
 * NA
 *
 * BUGS:
 * Lots, no checking of product type would generate core dump etc....
 *
 * AUTHOR:
 * Øystein Godøy, METNO/FOU, 27.04.2006 
 */

#include <readosisaf.h>

void readosisafheader(char **infile, char **description, int *year, int *month, int *day, int *hour, int *minute, int *xsize, int *ysize, int *zsize, double *ucs_ul_x, double *ucs_ul_y, double *ucs_dx, double *ucs_dy) {

    char *where="readosisafheader";
    char what[250];
    /*int i, j, k, size;*/
    int status;
    osihdf ipd;

    printf("\nOpen file: %s\n",*infile);

    /*
     * Open input file.
     */
    status = read_hdf5_product(*infile, &ipd, 1);
    if (status != 0) {
	sprintf(what,"Could not access OSIHDF file %s\n", *infile);
	errmsg(where,what);
	return;
    }

    /*
     * Get metadata
     */
    sprintf(*description,"%s",ipd.h.product);
    *year = ipd.h.year;
    *month = ipd.h.month;
    *day = ipd.h.day;
    *hour = ipd.h.hour;
    *minute = ipd.h.minute;
    *xsize = ipd.h.iw;
    *ysize = ipd.h.ih;
    *zsize = ipd.h.z;
    *ucs_ul_x = ipd.h.Bx;
    *ucs_ul_y = ipd.h.By;
    *ucs_dx = ipd.h.Ax;
    *ucs_dy = (double) ipd.h.Ay;

    /*
     * Do some cleaning
     */
    free_osihdf(&ipd);

    return;
}

