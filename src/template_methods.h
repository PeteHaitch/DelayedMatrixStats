#ifndef TEMPLATE_METHODS_H
#define TEMPLATE_METHODS_H

// TODO: Is it possible to make this return an IntegerVector::Iterator rather
//       than a IntegerVector?
template<class MPTR>
  Rcpp::IntegerVector process_subset_vector(Rcpp::RObject subset,
                                            MPTR mat,
                                            bool byrow) {

    if (subset.sexp_type() != NILSXP & subset.sexp_type() != INTSXP) {
      throw std::runtime_error("subset vector must be NULL or an integer vector");
    }

    const int& upper = (byrow ? mat->get_nrow() : mat->get_ncol());
    if (subset.sexp_type() == INTSXP) {
      Rcpp::IntegerVector sout(subset);
      for (auto sIt = sout.begin(); sIt != sout.end(); ++sIt) {
        if (*sIt < 0 || *sIt >= upper) {
          throw std::runtime_error("subset indices out of range");
        }
      }
      return sout;
    } else {
      // TODO: Is this the best way to do this? E.g., ALTREP makes 1:n type
      //       expressions really efficient at the R-level
      // NOTE: -1 to convert from R's 1-based indices to C's 0-based indices
      Rcpp::IntegerVector sout = Rcpp::seq_len(upper) - 1;
      return sout;
    }
  }

#endif