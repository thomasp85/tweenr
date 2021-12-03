#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/strings.hpp>
#include <cpp11/list.hpp>
#include <cpp11/list_of.hpp>
#include <cpp11/matrix.hpp>

#include "utils.h"

[[cpp11::register]]
cpp11::doubles numeric_at_interpolator(cpp11::doubles from, cpp11::doubles to,
                                       cpp11::doubles at, cpp11::strings ease) {
  R_xlen_t n = from.size();
  std::string easer = ease[0];
  cpp11::writable::doubles res(n);

  for (R_xlen_t i = 0; i < n; ++i) {
    double pos = ease_pos(at[i], easer);
    res[i] = from[i] + (to[i] - from[i]) * pos;
  }

  return res;
}
[[cpp11::register]]
cpp11::doubles_matrix<> colour_at_interpolator(cpp11::doubles_matrix<> from, cpp11::doubles_matrix<> to,
                                             cpp11::doubles at, cpp11::strings ease) {
  R_xlen_t n = from.nrow(), m = from.ncol();
  std::string easer = ease[0];
  cpp11::writable::doubles_matrix<> res(n, m);

  for (R_xlen_t i = 0; i < n; ++i) {
    double pos = ease_pos(at[i], easer);
    for (R_xlen_t j = 0; j < m; ++j) {
      res(i, j) = from(i, j) + (to(i, j) - from(i, j)) * pos;
    }
  }

  return res;
}
[[cpp11::register]]
cpp11::strings constant_at_interpolator(cpp11::strings from, cpp11::strings to,
                                        cpp11::doubles at, cpp11::strings ease) {
  R_xlen_t n = from.size();
  std::string easer = ease[0];
  cpp11::writable::strings res(n);

  for (R_xlen_t i = 0; i < n; ++i) {
    double pos = ease_pos(at[i], easer);
    res[i] = pos < 0.5 ? from[i] : to[i];
  }

  return res;
}
[[cpp11::register]]
cpp11::list list_at_interpolator(cpp11::list from, cpp11::list to,
                          cpp11::doubles at, cpp11::strings ease) {
  R_xlen_t n = from.size();
  std::string easer = ease[0];
  cpp11::writable::list res(n);

  for (R_xlen_t i = 0; i < n; ++i) {
    double pos = ease_pos(at[i], easer);
    res[i] = pos < 0.5 ? from[i] : to[i];
  }

  return res;
}
[[cpp11::register]]
cpp11::list numlist_at_interpolator(cpp11::list_of<cpp11::doubles> from, cpp11::list_of<cpp11::doubles> to,
                                    cpp11::doubles at, cpp11::strings ease) {
  R_xlen_t n = from.size();
  std::string easer = ease[0];
  cpp11::writable::list res(n);

  for (R_xlen_t i = 0; i < n; ++i) {
    cpp11::doubles state_from_vec = from[i];
    cpp11::doubles state_to_vec = to[i];
    state_from_vec = align_num_elem(state_from_vec, state_to_vec);
    state_to_vec = align_num_elem(state_to_vec, state_from_vec);
    double pos = ease_pos(at[i], easer);
    cpp11::writable::doubles state_vec(state_from_vec.size());
    for (R_xlen_t i = 0; i < state_from_vec.size(); ++i) {
      state_vec[i] = state_from_vec[i] + pos * (state_to_vec[i] - state_from_vec[i]);
    }
    res[i] = state_vec;
  }

  return res;
}
