#include "tf.h"

SEXP bflsa(SEXP Rindx, SEXP Ry, SEXP Rlam)
{

    // initialization
    int i;
    int j = 0;
    int *indx = INTEGER(Rindx);
    double *y = REAL(Ry);
    double lam = REAL(Rlam)[0];
    int n = length(Ry);
    double *beta = malloc(n*sizeof(double));
    double *suby = y;
    double *subbeta = beta;
    SEXP Rbeta;

    // perform blockwise flsa
    for(i=0; i<n-1; i++)
    {
        j++;
        if(indx[i] != indx[i+1])
        {
            tf_dp(j, suby, lam, subbeta);
            subbeta = beta+i+1;
            suby = y+i+1;
            j = 0;
        }
    }
    tf_dp(j+1, suby, lam, subbeta);


    PROTECT(Rbeta = allocVector(REALSXP, n));
    for (i = 0; i < n; i++)
        REAL(Rbeta)[i] = beta[i];
    UNPROTECT(1);

    free(beta);

    return Rbeta;

}

SEXP bwflsa(SEXP Rindx, SEXP Ry, SEXP Rw, SEXP Rlam)
{

    // initialization
    int i;
    int j = 0;
    int *indx = INTEGER(Rindx);
    double *y = REAL(Ry);
    double *w = REAL(Rw);
    double lam = REAL(Rlam)[0];
    int n = length(Ry);
    double *beta = malloc(n*sizeof(double));
    double *suby = y;
    double *subbeta = beta;
    double *subw = w;
    SEXP Rbeta;

    // perform blockwise flsa
    for(i=0; i<n-1; i++)
    {
        j++;
        if(indx[i] != indx[i+1])
        {
            tf_dp_weight(j, suby, subw, lam, subbeta);
            subbeta = beta+i+1;
            suby = y+i+1;
            subw = w+i+1;
            j = 0;
        }
    }
    tf_dp_weight(j+1, suby, subw, lam, subbeta);

    PROTECT(Rbeta = allocVector(REALSXP, n));
    for (i = 0; i < n; i++)
        REAL(Rbeta)[i] = beta[i];
    UNPROTECT(1);

    free(beta);

    return Rbeta;

}






