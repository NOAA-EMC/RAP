#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <limits.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * write a grib-2 file 
 *
 * sec0..sec4 predefined sections 0 to 4
 * data[] = values to encode into grib
 * ndata = size of data
 * out = output file
 *
 */

int simple_grib_out(unsigned char **sec, float *data, unsigned int ndata, 
   int use_scale, int dec_scale, int bin_scale, int wanted_bits, 
   int max_bits, struct seq_file *out) {

    unsigned int n_defined;
    int i;
    unsigned char *sec0, *sec1, *sec2 , *sec3, *sec4, *sec5, *sec6, *sec7;

    /* required passed sections */
    sec0 = sec[0];
    sec1 = sec[1];
    sec2 = sec[2];
    sec3 = sec[3];
    sec4 = sec[4];

    /* make a sections 5-7 */
    n_defined = ndata;
//  fprintf(stderr, "simple_pk 1: ndata=%u\n", ndata);

    sec6 = mk_bms(data, &n_defined);			// make bitmap section
//  fprintf(stderr, "simple_pk 1: n_defined=%u\n", n_defined);
    mk_sec5and7(data, n_defined, &sec5, &sec7,use_scale,dec_scale,bin_scale, 
        wanted_bits, max_bits);		// make sec 5 and 7

    i = wrt_sec(sec0, sec1, sec2, sec3, sec4, sec5, sec6, sec7, out);

    free(sec5);
    free(sec6);
    free(sec7);

    return i;
}

/*
 * make sec 5 and 7 using simple packing
 */


int mk_sec5and7(float *data, unsigned int n, unsigned char **sec5, unsigned char **sec7, 
	int use_scale, int dec_scale, int bin_scale, int wanted_bits, int max_bits) {

    float min_val, max_val, ref, ncep_min_val, mn, mx;
    int nbits, binary_scale, j;
    double frange, scale, dec_factor;
    size_t sec5_size, sec7_size;
    unsigned char *p;
    unsigned int i, k;

/* single threaded
    for (max_val = min_val = data[0], i = 1; i < n; i++) {
	if (min_val > data[i]) min_val = data[i];
        if (max_val < data[i]) max_val = data[i];
    }
*/

    min_val = max_val = data[0];
#pragma omp parallel private(mn, mx, i)
    {
        mn = mx = data[0];

#pragma omp for schedule(static) nowait
        for (i = 1; i < n; i++) {
            mn = (mn > data[i]) ? data[i] : mn;
            mx = (mx < data[i]) ? data[i] : mx;
        }

#pragma omp critical
        {
            if (min_val > mn) min_val = mn;
            if (max_val < mx) max_val = mx;
        }
    }

    binary_scale = bin_scale;
    ncep_min_val = min_val;

    if (use_scale == 0) {
        /* ecmwf style */
        ref = min_val;
        frange = max_val - ref;
        dec_scale = 0;
        if (frange != 0.0) {
            frexp(frange, &j);
            binary_scale = j - wanted_bits;
            nbits = wanted_bits;
            scale = ldexp(1.0, -binary_scale);
            frange = floor((max_val-ref)*scale + 0.5);
            frexp(frange, &j);
            if (j != nbits) binary_scale++;
        }
        else {
            binary_scale = nbits = 0;
            scale = 1;
        }
    }
    else {
        if (dec_scale) {
            dec_factor = Int_Power(10.0, -dec_scale);
            min_val *= dec_factor;
            max_val *= dec_factor;
// #pragma omp parallel for schedule(static)
            for (i = 0; i < n; i++) {
                data[i] *= dec_factor;
            }
        }

        scale = ldexp(1.0, -binary_scale);
        ref = min_val;
        j = (int) ((max_val - ref)*scale + 0.5);
        frange = (double) j;
        frexp(frange, &nbits);
        if (nbits > max_bits) {
            binary_scale += (nbits - max_bits);
            nbits = max_bits;
        }
    }
    if (binary_scale) {
        scale = ldexp(1.0, -binary_scale);
// #pragma omp parallel for schedule(static)
        for (i = 0; i < n; i++) {
            data[i] = (data[i] - ref)*scale;
        }
    }
    else {
// #pragma omp parallel for schedule(static)
        for (i = 0; i < n; i++) {
            data[i] = data[i] - ref;
        }
    }

    sec5_size = 21;
    sec7_size = 5 + (nbits * (n / 8)) + (nbits * (n % 8) + 7) / 8;

    // section 7
    *sec7 = p = (unsigned char *) malloc(sec7_size);
    uint_char(sec7_size, p);
    p[4] = 7;

//    single thread version
//    flist2bitstream(data,p + 5,n,nbits);

//  for OpenMP, want different threads not to share r/w memory
//    keep on different cache lines
//     typically a cache line is 512 or 1024 bits
//     no problem if CACHE_LINE_BITS is a multiple of the true size of 
//     cache line bits.
//     can get some false sharing because p+5 may not be on cache boundary :(

#pragma omp parallel for private(i,k) schedule(static)
    for (i = 0; i < n; i+= CACHE_LINE_BITS) {
	k  = n - i;
	if (k > CACHE_LINE_BITS) k = CACHE_LINE_BITS;
        flist2bitstream(data + i, p + 5 + (i/8)*nbits, k, nbits);
    }

    // section 5

    // fix for buggy NCEP decoders
    // for constant fields, they ignore the decimal scaling
    if (nbits == 0) {
	dec_scale = binary_scale = 0;
	ref = ncep_min_val;
    }

    *sec5 = p = (unsigned char *) malloc(sec5_size);
    uint_char(sec5_size, p);		// length of section 5
    p[4] = 5;				// section 5
    uint_char(n, p+5);			// number of defined points
    uint2_char(0,p+9);			// template 5.0
    flt2ieee(ref,p+11);			// ieee reference value
    int2_char(binary_scale,p+15);
    int2_char(-dec_scale,p+17);
    p[19] = nbits;
    p[20] = 0;				// template 5.1 - set to floating

    return 0;
}

