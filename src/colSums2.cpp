#include "DelayedMatrixStats.h"

#include <numeric>

// Computing sum of **first column only**
// TODO: Generalise to computing all column sums, then to selected cols, then
//       to selected cols and rows
template <typename T, class V, class X>
Rcpp::RObject colSums2_internal (X x,
                                 Rcpp::RObject cols,
                                 Rcpp::RObject na_rm) {

  // Checking matrix dimensions
  const size_t nrow = x->get_nrow();
  // TODO: Handle NULL cols
  Rcpp::IntegerVector cols_out = process_subset_vector(cols, x, false);
  const size_t col_len = cols_out.size();

  // Checking scalars
  // TODO: This check should be equivalent to `asLogicalNoNA()` in matrixStats
  //       https://github.com/HenrikBengtsson/matrixStats/blob/master/src/rowSums2.c#L25
  if (na_rm.sexp_type() != LGLSXP || LENGTH(na_rm) != 1) {
    throw std::runtime_error("na.rm specification should be a logical scalar");
  }
  const bool NA_RM = Rcpp::LogicalVector(na_rm)[0];

  // Setting up output
  V output(col_len);
  // Setting up intermediate iterator to represent a single column of the input
  // matrix
  V input(nrow);

  auto output_it = output.begin();
  for (const auto& col_it : cols_out) {
    // TODO: An attempt using get_const_col()
    x->get_col(col_it, input.begin());

    // TODO: Move outside of loop?
    if (NA_RM) {
      // TODO: Will need to either:
      //       (1) [ideally] add a `na.rm` arg to Rcpp::algorithm::sum();
      //           see https://github.com/RcppCore/Rcpp/issues/757
      //       (2)  remove NA elements before passing to Rcpp::algorith::sum()
      // An attempt at (2)
      // TODO: Is a Rcpp::clone(input) needed? See
      //       https://github.com/RcppCore/Rcpp/blob/master/inst/include/Rcpp/sugar/functions/median.h#L137
      V input_no_na = Rcpp::na_omit(input);
      Rcpp::Rcout << "NA_RM is TRUE" << std::endl;
      auto total = Rcpp::algorithm::sum(input_no_na.begin(), input_no_na.end());
      *output_it = total;
      ++output_it;
    } else {
      Rcpp::Rcout << "NA_RM is FALSE" << std::endl;
      auto total = Rcpp::algorithm::sum(input.begin(), input.end());
      *output_it = total;
      ++output_it;
    }
  }
  return output;
}

SEXP colSums2 (SEXP x, SEXP cols, SEXP na_rm) {
  BEGIN_RCPP

  auto x_type = beachmat::find_sexp_type(x);
  if (x_type == INTSXP) {
    auto x_ptr = beachmat::create_integer_matrix(x);
    return colSums2_internal<int, Rcpp::IntegerVector>(x_ptr.get(),
                                                       cols,
                                                       na_rm);
  } else if (x_type == REALSXP) {
    auto x_ptr = beachmat::create_numeric_matrix(x);
    return colSums2_internal<double, Rcpp::NumericVector>(x_ptr.get(),
                                                          cols,
                                                          na_rm);
  } else {
    throw std::runtime_error("unacceptable matrix type");
  }

  END_RCPP
}

// Misc. notes
// - Rcp::sugar operates on Vector (e.g. Rcpp::NumericVector)
// - Rcpp::algorithm operate on Iterator (e.g., Rcpp::NumericVector::Iterator)
// - https://github.com/RcppCore/Rcpp/issues/426
// - https://github.com/RcppCore/Rcpp/pull/428
// - https://github.com/RcppCore/Rcpp/pull/481
// - http://thecoatlessprofessor.com/programming/rcpp/unofficial-rcpp-api-docs
// - http://gallery.rcpp.org/articles/rcpp-algorithm/