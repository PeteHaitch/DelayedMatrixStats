#ifndef DELAYEDMATRIXSTATS_H
#define DELAYEDMATRIXSTATS_H

#include "Rcpp.h"
// NOTE: Not including logical matrix (yet)
#include "beachmat/numeric_matrix.h"
#include "beachmat/integer_matrix.h"

extern "C" {
  SEXP colSums2 (SEXP, SEXP, SEXP);
}

#include "template_methods.h"

#endif
