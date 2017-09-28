#include "DelayedMatrixStats.h"
#define REGISTER(x, i) {#x, (DL_FUNC) &x, i}

extern "C" {

  static const R_CallMethodDef all_call_entries[] = {
    REGISTER(colSums2, 3),
    {0}
  };

  void attribute_visible R_init_DelayedMatrixStats(DllInfo *dll) {
    R_registerRoutines(dll, NULL, all_call_entries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
  }

}
