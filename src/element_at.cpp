#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/strings.hpp>
#include <cpp11/list.hpp>
#include <cpp11/list_of.hpp>
#include <cpp11/data_frame.hpp>
#include <cpp11/matrix.hpp>

#include "utils.h"

using namespace cpp11::literals;

[[cpp11::register]]
cpp11::writable::doubles numeric_element_at_interpolator(cpp11::doubles data, cpp11::integers group, cpp11::doubles time, double at, cpp11::strings ease) {
  cpp11::writable::doubles tweendata;

  for (R_xlen_t i = 1; i < data.size(); ++i) {
    if ((group[i - 1] == group[i] && time[i - 1] < at && time[i] >= at) ||
        ((i == 1 || group[i - 2] != group[i - 1]) && time[i - 1] == at)) {
      double p = ease_pos((at - time[i - 1]) / (time[i] - time[i - 1]), ease[i - 1]);
      tweendata.push_back(data[i - 1] + p * (data[i] - data[i - 1]));
    }
  }

  return tweendata;
}

[[cpp11::register]]
cpp11::writable::data_frame colour_element_at_interpolator(cpp11::doubles_matrix<> data, cpp11::integers group, cpp11::doubles time, double at, cpp11::strings ease) {
  cpp11::writable::doubles tweendata1;
  cpp11::writable::doubles tweendata2;
  cpp11::writable::doubles tweendata3;
  cpp11::writable::doubles tweendata4;

  for (R_xlen_t i = 1; i < data.nrow(); ++i) {
    if ((group[i - 1] == group[i] && time[i - 1] < at && time[i] >= at) ||
        ((i == 1 || group[i - 2] != group[i - 1]) && time[i - 1] == at)) {
      double p = ease_pos((at - time[i - 1]) / (time[i] - time[i - 1]), ease[i - 1]);
      tweendata1.push_back(data(i - 1, 0) + p * (data(i, 0) - data(i - 1, 0)));
      tweendata2.push_back(data(i - 1, 1) + p * (data(i, 1) - data(i - 1, 1)));
      tweendata3.push_back(data(i - 1, 2) + p * (data(i, 2) - data(i - 1, 2)));
      tweendata4.push_back(data(i - 1, 3) + p * (data(i, 3) - data(i - 1, 3)));

    }
  }

  return cpp11::writable::data_frame({
    "L"_nm = tweendata1,
    "a"_nm = tweendata2,
    "b"_nm = tweendata3,
    "alpha"_nm = tweendata4
  });
}

[[cpp11::register]]
cpp11::writable::strings constant_element_at_interpolator(cpp11::strings data, cpp11::integers group, cpp11::doubles time, double at, cpp11::strings ease) {
  cpp11::writable::strings tweendata;

  for (R_xlen_t i = 1; i < data.size(); ++i) {
    if ((group[i - 1] == group[i] && time[i - 1] < at && time[i] >= at) ||
        ((i == 1 || group[i - 2] != group[i - 1]) && time[i - 1] == at)) {
      double p = ease_pos((at - time[i - 1]) / (time[i] - time[i - 1]), ease[i - 1]);
      tweendata.push_back(p < 0.5 ? data[i - 1] : data[i]);
    }
  }

  return tweendata;
}

[[cpp11::register]]
cpp11::writable::list list_element_at_interpolator(cpp11::list data, cpp11::integers group, cpp11::doubles time, double at, cpp11::strings ease) {
  cpp11::writable::list tweendata;

  for (R_xlen_t i = 1; i < data.size(); ++i) {
    if ((group[i - 1] == group[i] && time[i - 1] < at && time[i] >= at) ||
        ((i == 1 || group[i - 2] != group[i - 1]) && time[i - 1] == at)) {
      double p = ease_pos((at - time[i - 1]) / (time[i] - time[i - 1]), ease[i - 1]);
      tweendata.push_back(p < 0.5 ? data[i - 1] : data[i]);
    }
  }

  return tweendata;
}

[[cpp11::register]]
cpp11::writable::list numlist_element_at_interpolator(cpp11::list_of<cpp11::doubles> data, cpp11::integers group, cpp11::doubles time, double at, cpp11::strings ease) {
  cpp11::writable::list tweendata;

  for (R_xlen_t i = 1; i < data.size(); ++i) {
    if ((group[i - 1] == group[i] && time[i - 1] < at && time[i] >= at) ||
        ((i == 1 || group[i - 2] != group[i - 1]) && time[i - 1] == at)) {
      double p = ease_pos((at - time[i - 1]) / (time[i] - time[i - 1]), ease[i - 1]);
      cpp11::doubles state_from_vec = data[i - 1];
      cpp11::doubles state_to_vec = data[i];
      state_from_vec = align_num_elem(state_from_vec, state_to_vec);
      state_to_vec = align_num_elem(state_to_vec, state_from_vec);
      cpp11::writable::doubles state_vec(state_from_vec.size());
      for (R_xlen_t i = 0; i < state_from_vec.size(); ++i) {
        state_vec[i] = state_from_vec[i] + p * (state_to_vec[i] - state_from_vec[i]);
      }
      tweendata.push_back(state_vec);
    }
  }

  return tweendata;
}

[[cpp11::register]]
cpp11::writable::strings phase_element_at_interpolator(cpp11::strings data, cpp11::integers group, cpp11::doubles time, double at, cpp11::strings ease) {
  cpp11::writable::strings tweendata;

  for (R_xlen_t i = 1; i < data.size(); ++i) {
    if ((group[i - 1] == group[i] && time[i - 1] < at && time[i] >= at) ||
        ((i == 1 || group[i - 2] != group[i - 1]) && time[i - 1] == at)) {
      if ((at == time[i - 2] && !(data[i - 1] == "enter")) || (at == time[i] && !(data[i] == "exit"))) {
        tweendata.push_back("raw");
      } else{
        tweendata.push_back(data[i - 1] == "enter" ? "enter" : data[i] == "exit" ? "exit" : data[i - 1] == "static" ? "static" : "transition");
      }
    }
  }

  return tweendata;
}
