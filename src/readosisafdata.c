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

void readosisafdata(char **infile, double *mymatrix) {

    char *where="readosisaf";
    char what[250];
    int i, j, k, status;
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

    /*
     * Get the data
     */
    i = 0;
    for (j=0;j<ipd.h.ih;j++) {
	for (k=0;k<ipd.h.iw;k++) {
	    mymatrix[i] = ((float *) ipd.d[0].data)[i];
	    i++;
	}
    }

    /*
     * Do some cleaning
     */
    free_osihdf(&ipd);

    return;
}

