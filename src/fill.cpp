#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/strings.hpp>
#include <cpp11/list.hpp>
#include <cpp11/list_of.hpp>
#include <cpp11/matrix.hpp>

#include "utils.h"

[[cpp11::register]]
cpp11::doubles numeric_fill_interpolator(cpp11::doubles data, cpp11::strings ease) {
  cpp11::writable::doubles res(data.size());
  std::fill(res.begin(), res.end(), R_NaReal);
  int last = -1;
  std::string easer = ease[0];

  for (R_xlen_t i = 0; i < data.size(); ++i) {
    if (cpp11::is_na(data[i])) continue;
    if (last != -1) {
      std::vector<double> easepos = ease_seq(easer, i - last);
      for (size_t j = 1; j < easepos.size(); ++j) {
        res[last + j] = data[last] + easepos[j] * (data[i] - data[last]);
      }
    }
    res[i] = data[i];
    last = i;
  }

  return res;
}
[[cpp11::register]]
cpp11::doubles_matrix<> colour_fill_interpolator(cpp11::doubles_matrix<> data, cpp11::strings ease) {
  cpp11::writable::doubles_matrix<> res(data.nrow(), data.ncol());
  for (int i = 0; i < res.nrow(); ++i) {
    for (int j = 0; j < res.ncol(); ++j) {
      res(i, j) = R_NaReal;
    }
  }
  int last = -1;
  std::string easer = ease[0];

  for (R_xlen_t i = 0; i < data.nrow(); ++i) {
    if (cpp11::is_na(data(i, 0))) continue;
    if (last != -1) {
      std::vector<double> easepos = ease_seq(easer, i - last);
      for (size_t j = 1; j < easepos.size(); ++j) {
        for (R_xlen_t k = 0; k < data.ncol(); ++k) {
          res(last + j, k) = data(last, k) + easepos[j] * (data(i, k) - data(last, k));
        }
      }
    }
    for (R_xlen_t k = 0; k < data.ncol(); ++k) {
      res(i, k) = data(i, k);
    }
    last = i;
  }

  return res;
}
[[cpp11::register]]
cpp11::strings constant_fill_interpolator(cpp11::strings data, cpp11::strings ease) {
  cpp11::writable::strings res(data.size());
  std::fill(res.begin(), res.end(), R_NaString);
  int last = -1;
  std::string easer = ease[0];

  for (R_xlen_t i = 0; i < data.size(); ++i) {
    if (cpp11::is_na(data[i])) continue;
    if (last != -1) {
      std::vector<double> easepos = ease_seq(easer, i - last);
      for (size_t j = 1; j < easepos.size(); ++j) {
        res[last + j] = easepos[j] < 0.5 ? data[last] : data[i];
      }
    }
    res[i] = data[i];
    last = i;
  }

  return res;
}
[[cpp11::register]]
cpp11::list list_fill_interpolator(cpp11::list data, cpp11::strings ease) {
  cpp11::writable::list res(data.size());
  std::fill(res.begin(), res.end(), R_NilValue);
  int last = -1;
  std::string easer = ease[0];

  for (R_xlen_t i = 0; i < data.size(); ++i) {
    if (data[i] == R_NilValue) continue;
    if (last != -1) {
      std::vector<double> easepos = ease_seq(easer, i - last);
      for (size_t j = 1; j < easepos.size(); ++j) {
        res[last + j] = easepos[j] < 0.5 ? data[last] : data[i];
      }
    }
    res[i] = data[i];
    last = i;
  }
  return res;
}
[[cpp11::register]]
cpp11::list numlist_fill_interpolator(cpp11::list_of<cpp11::doubles> data, cpp11::strings ease) {
  cpp11::writable::list res(data.size());
  std::fill(res.begin(), res.end(), R_NilValue);
  int last = -1;
  std::string easer = ease[0];

  for (R_xlen_t i = 0; i < data.size(); ++i) {
    if (data[i] == R_NilValue) continue;
    if (last != -1) {
      std::vector<double> easepos = ease_seq(easer, i - last);
      cpp11::doubles state_from_vec = data[last];
      cpp11::doubles state_to_vec = data[i];
      state_from_vec = align_num_elem(state_from_vec, state_to_vec);
      state_to_vec = align_num_elem(state_to_vec, state_from_vec);
      res[last] = data[last];
      for (size_t j = 1; j < easepos.size(); ++j) {
        cpp11::writable::doubles state_vec(state_from_vec.size());
        for (R_xlen_t k = 0; k < state_from_vec.size(); ++k) {
          state_vec[k] = state_from_vec[k] + easepos[j] * (state_to_vec[k] - state_from_vec[k]);
        }
        res[last + j] = state_vec;
      }
    }
    res[i] = data[i];
    last = i;
  }
  return res;
}
