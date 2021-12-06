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
cpp11::writable::data_frame numeric_element_interpolator(cpp11::doubles data, cpp11::integers group, cpp11::integers frame, cpp11::strings ease) {
  cpp11::writable::doubles tweendata;
  cpp11::writable::integers tweengroup;
  cpp11::writable::integers tweenframe;
  int current_group = group[0];

  R_xlen_t i;
  for (i = 1; i < data.size(); ++i) {
    if (current_group == group[i]) {
      int nframes = frame[i] - frame[i-1];
      std::vector<double> ease_points = ease_seq(ease[i-1], nframes);
      for (size_t j = 0; j < ease_points.size(); ++j) {
        tweendata.push_back(data[i - 1] + ease_points[j] * (data[i] - data[i - 1]));
        tweengroup.push_back(current_group);
        tweenframe.push_back(j + frame[i-1]);
      }
    } else {
      tweendata.push_back(data[i - 1]);
      tweengroup.push_back(current_group);
      tweenframe.push_back(frame[i-1]);
      current_group = group[i];
    }
  }

  tweendata.push_back(data[i - 1]);
  tweengroup.push_back(current_group);
  tweenframe.push_back(frame[i-1]);

  return cpp11::writable::data_frame({
    "data"_nm = tweendata,
    "group"_nm = tweengroup,
    "frame"_nm = tweenframe
  });
}

[[cpp11::register]]
cpp11::writable::data_frame colour_element_interpolator(cpp11::doubles_matrix<> data, cpp11::integers group, cpp11::integers frame, cpp11::strings ease) {
  cpp11::writable::doubles tweendata1;
  cpp11::writable::doubles tweendata2;
  cpp11::writable::doubles tweendata3;
  cpp11::writable::doubles tweendata4;
  cpp11::writable::integers tweengroup;
  cpp11::writable::integers tweenframe;
  int current_group = group[0];

  R_xlen_t i;
  for (i = 1; i < data.nrow(); ++i) {
    if (current_group == group[i]) {
      int nframes = frame[i] - frame[i-1];
      std::vector<double> ease_points = ease_seq(ease[i-1], nframes);
      for (size_t j = 0; j < ease_points.size(); ++j) {
        tweendata1.push_back(data(i - 1, 0) + ease_points[j] * (data(i, 0) - data(i - 1, 0)));
        tweendata2.push_back(data(i - 1, 1) + ease_points[j] * (data(i, 1) - data(i - 1, 1)));
        tweendata3.push_back(data(i - 1, 2) + ease_points[j] * (data(i, 2) - data(i - 1, 2)));
        tweendata4.push_back(data(i - 1, 3) + ease_points[j] * (data(i, 3) - data(i - 1, 3)));
        tweengroup.push_back(current_group);
        tweenframe.push_back(j + frame[i-1]);
      }
    } else {
      tweendata1.push_back(data(i - 1, 0));
      tweendata2.push_back(data(i - 1, 1));
      tweendata3.push_back(data(i - 1, 2));
      tweendata4.push_back(data(i - 1, 3));
      tweengroup.push_back(current_group);
      tweenframe.push_back(frame[i-1]);
      current_group = group[i];
    }
  }
  tweendata1.push_back(data(i - 1, 0));
  tweendata2.push_back(data(i - 1, 1));
  tweendata3.push_back(data(i - 1, 2));
  tweendata4.push_back(data(i - 1, 3));
  tweengroup.push_back(current_group);
  tweenframe.push_back(frame[i-1]);

  return cpp11::writable::data_frame({
    "data1"_nm = tweendata1,
    "data2"_nm = tweendata2,
    "data3"_nm = tweendata3,
    "data4"_nm = tweendata4,
    "group"_nm = tweengroup,
    "frame"_nm = tweenframe
  });
}

[[cpp11::register]]
cpp11::writable::data_frame constant_element_interpolator(cpp11::strings data, cpp11::integers group, cpp11::integers frame, cpp11::strings ease) {
  cpp11::writable::strings tweendata;
  cpp11::writable::integers tweengroup;
  cpp11::writable::integers tweenframe;
  int current_group = group[0];

  R_xlen_t i;
  for (i = 1; i < data.size(); ++i) {
    if (current_group == group[i]) {
      int nframes = frame[i] - frame[i-1];
      std::vector<double> ease_points = ease_seq(ease[i-1], nframes);
      for (size_t j = 0; j < ease_points.size(); ++j) {
        if (ease_points[j] < 0.5) {
          tweendata.push_back(data[i - 1]);
        } else {
          tweendata.push_back(data[i]);
        }
        tweengroup.push_back(current_group);
        tweenframe.push_back(j + frame[i-1]);
      }
    } else {
      tweendata.push_back(data[i - 1]);
      tweengroup.push_back(current_group);
      tweenframe.push_back(frame[i-1]);
      current_group = group[i];
    }
  }

  tweendata.push_back(data[i - 1]);
  tweengroup.push_back(current_group);
  tweenframe.push_back(frame[i-1]);

  return cpp11::writable::data_frame({
    "data"_nm = tweendata,
    "group"_nm = tweengroup,
    "frame"_nm = tweenframe
  });
}

[[cpp11::register]]
cpp11::writable::data_frame list_element_interpolator(cpp11::list data, cpp11::integers group, cpp11::integers frame, cpp11::strings ease) {
  cpp11::writable::list tweendata;
  cpp11::writable::integers tweengroup;
  cpp11::writable::integers tweenframe;
  int current_group = group[0];

  R_xlen_t i;
  for (i = 1; i < data.size(); ++i) {
    if (current_group == group[i]) {
      int nframes = frame[i] - frame[i-1];
      std::vector<double> ease_points = ease_seq(ease[i-1], nframes);
      for (size_t j = 0; j < ease_points.size(); ++j) {
        if (ease_points[j] < 0.5) {
          tweendata.push_back(data[i - 1]);
        } else {
          tweendata.push_back(data[i]);
        }
        tweengroup.push_back(current_group);
        tweenframe.push_back(j + frame[i-1]);
      }
    } else {
      tweendata.push_back(data[i - 1]);
      tweengroup.push_back(current_group);
      tweenframe.push_back(frame[i-1]);
      current_group = group[i];
    }
  }

  tweendata.push_back(data[i - 1]);
  tweengroup.push_back(current_group);
  tweenframe.push_back(frame[i-1]);

  return cpp11::writable::data_frame({
    "data"_nm = tweendata,
    "group"_nm = tweengroup,
    "frame"_nm = tweenframe
  });
}

[[cpp11::register]]
cpp11::writable::data_frame numlist_element_interpolator(cpp11::list_of<cpp11::doubles> data, cpp11::integers group, cpp11::integers frame, cpp11::strings ease) {
  cpp11::writable::list tweendata;
  cpp11::writable::integers tweengroup;
  cpp11::writable::integers tweenframe;
  int current_group = group[0];

  R_xlen_t i;
  for (i = 1; i < data.size(); ++i) {
    if (current_group == group[i]) {
      int nframes = frame[i] - frame[i-1];
      std::vector<double> ease_points = ease_seq(ease[i-1], nframes);
      cpp11::doubles state_from_vec = data[i - 1];
      cpp11::doubles state_to_vec = data[i];
      state_from_vec = align_num_elem(state_from_vec, state_to_vec);
      state_to_vec = align_num_elem(state_to_vec, state_from_vec);
      for (size_t j = 0; j < ease_points.size(); ++j) {
        cpp11::writable::doubles state_vec(state_from_vec.size());
        for (R_xlen_t k = 0; k < state_from_vec.size(); ++k) {
          state_vec[k] = state_from_vec[k] + ease_points[j] * (state_to_vec[k] - state_from_vec[k]);
        }
        tweendata.push_back(state_vec);
        tweengroup.push_back(current_group);
        tweenframe.push_back(j + frame[i-1]);
      }
    } else {
      tweendata.push_back(data[i - 1]);
      tweengroup.push_back(current_group);
      tweenframe.push_back(frame[i-1]);
      current_group = group[i];
    }

  }
  tweendata.push_back(data[i - 1]);
  tweengroup.push_back(current_group);
  tweenframe.push_back(frame[i-1]);

  return cpp11::writable::data_frame({
    "data"_nm = tweendata,
    "group"_nm = tweengroup,
    "frame"_nm = tweenframe
  });
}

[[cpp11::register]]
cpp11::writable::data_frame phase_element_interpolator(cpp11::strings data, cpp11::integers group, cpp11::integers frame, cpp11::strings ease) {
  cpp11::writable::strings tweendata;
  cpp11::writable::integers tweengroup;
  cpp11::writable::integers tweenframe;
  int current_group = group[0];

  R_xlen_t i;
  for (i = 1; i < data.size(); ++i) {
    if (current_group == group[i]) {
      int nframes = frame[i] - frame[i-1];
      cpp11::r_string type = data[i - 1] == "enter" ? "enter" : data[i] == "exit" ? "exit" : data[i - 1] == "static" ? "static" : "transition";
      for (int j = 0; j < nframes; ++j) {
        if (j == 0 && (type == "transition" || type == "exit")) {
          tweendata.push_back("raw");
        } else {
          tweendata.push_back(type);
        }
        tweengroup.push_back(current_group);
        tweenframe.push_back(j + frame[i-1]);
      }
    } else {
      tweendata.push_back(data[i - 1]);
      tweengroup.push_back(current_group);
      tweenframe.push_back(frame[i-1]);
      current_group = group[i];
    }

  }
  tweendata.push_back(data[i - 1]);
  tweengroup.push_back(current_group);
  tweenframe.push_back(frame[i-1]);

  return cpp11::writable::data_frame({
    "data"_nm = tweendata,
    "group"_nm = tweengroup,
    "frame"_nm = tweenframe
  });
}
