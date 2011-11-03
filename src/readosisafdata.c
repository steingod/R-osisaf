/*
 * NAME:
 * readosisaf.c
 *
 * PURPOSE:
 * To read OSISAF HDF5 files.
 *
 * NOTES:
 * Should check the data type of the field to be read as the OSIHDF5
 * format may contain any combination of data fields.
 *
 * BUGS:
 * Lots, no checking of product type would generate core dump etc....
 *
 * AUTHOR:
 * Øystein Godøy, METNO/FOU, 27.04.2006 
 *
 * Øystein Godøy, METNO/FOU, 2011-11-02: returns all data layers...
 *
 * $Id: readosisafdata.c,v 1.2 2011-11-03 09:02:29 steingod Exp $
 */

#include <readosisaf.h>

void readosisafdata(char **infile, double *mymatrix) {

    char *where="readosisaf";
    char what[250];
    int i, j, k, l, imgsize, status;
    osihdf ipd;

    printf("\nOpen file: %s\n",*infile);

    /*
     * Open input file.
     */
    status = read_hdf5_product(*infile, &ipd, 0);
    if (status != 0) {
	sprintf(what,"Could not access OSIHDF file %s\n", *infile);
	errmsg(where,what);
	return;
    }

    imgsize = ipd.h.iw*ipd.h.ih;

    /*
     * Get the data
     */
    for (l=0;l<ipd.h.z;l++) {
        i = 0;
        for (j=0;j<ipd.h.ih;j++) {
            for (k=0;k<ipd.h.iw;k++) {
                mymatrix[l*imgsize+i] = ((float *) ipd.d[l].data)[i];
                i++;
            }
        }
    }

    /*
     * Do some cleaning
     */
    free_osihdf(&ipd);

    return;
}

