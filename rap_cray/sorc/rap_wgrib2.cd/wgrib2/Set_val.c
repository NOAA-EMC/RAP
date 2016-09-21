#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * Set_ijval
 *
 *  Routine to set a grid point value
 *
 * 2/2009: Public Domain: Wesley Ebisuzaki
 * 5/2014: staggered grid support, added set_ival
 *
 */


extern int decode, latlon, scan;
extern double *lat, *lon;
extern int nx, ny;
extern unsigned int npnts;

/*
 * HEADER:100:set_ijval:misc:3:sets grid point value X=ix Y=iy Z=val
 */

int f_set_ijval(ARG3) {

    unsigned int i;
    struct local_struct {
        unsigned int ix, iy;
	float val;
    };
    struct local_struct *save;

    unsigned int x,y;

    if (mode == -1) {
        decode = 1;
        *local = save = (struct local_struct *) malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("memory allocation f_ijval","");

        save->ix = atol(arg1) - 1;
        save->iy = atol(arg2) - 1;
        save->val = atof(arg3);

	if (save->ix < 0) fatal_error_i("ijval: ix value (%d) should be >= 1", save->ix + 1);
	if (save->iy < 0) fatal_error_i("ijval: iy value (%d) should be >= 1", save->iy + 1);
    }
    else if (mode == -2) {
	free(*local);
    }
    if (mode < 0) return 0;
    save = (struct local_struct *) *local;

    x = save->ix;
    y = save->iy;

    if (GDS_Scan_staggered(scan)) fatal_error("ijval: does not support staggered grid","");
    i = x + y*nx;
    if (i < ndata) data[x + y*nx] = save->val;
    else fatal_error_ii("ijval: failed with (%ux%u)",x,y);
    return 0;
}

/*
 * HEADER:100:set_ival:misc:2:sets grid point value X=i1:i2:.. Y=va1:val2:.. grid[i1] = val1,etc
 */

int f_set_ival(ARG2) {
    int icnt, vcnt;
    int err1, err2;
    float val;
    unsigned int i;

    if (mode == -1) {
        decode = 1;
    }
    if (mode < 0) return 0;

    err1 = sscanf(arg1,"%u%n", &i, &icnt);
    err2 = sscanf(arg2,"%f%n", &val, &vcnt);
    while (err1 == 1 && err2 == 1) {
// fprintf(stderr,"set_ival i=%u v=%f\n",i,val);        
	if (i != 0 && i <= ndata) data[i-1] = val;
	else fatal_error_ii("set_ival: i=%u ndata=%u", i, ndata);
	arg1 += icnt;
	arg2 += vcnt;
	err1 = sscanf(arg1,":%u%n", &i, &icnt);
	err2 = sscanf(arg2,":%f%n", &val, &vcnt);
    }
    if (err1 != err2) fatal_error_ii("set_ival: size of list do not match %d %d",err1,err2);
    return 0;
}
