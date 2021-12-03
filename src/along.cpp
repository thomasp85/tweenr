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
cpp11::writable::data_frame numeric_along_interpolator(cpp11::doubles data,
                                                       cpp11::integers group,
                                                       cpp11::doubles time,
                                                       bool history,
                                                       bool keep_last,
                                                       cpp11::doubles frames,
                                                       cpp11::strings ease) {
  cpp11::writable::doubles tweendata;
  cpp11::writable::integers tweengroup;
  cpp11::writable::integers tweenframe;
  std::string easer = ease[0];

  for (int i = 0; i < frames.size(); ++i) {
    double frame_time = frames[i];
    for (R_xlen_t j = 0; j < data.size(); ++j) {
      bool last = j == data.size() - 1;
      R_xlen_t jj = last ? j : j + 1;
      bool before = time[j] <= frame_time;
      bool after = time[jj] > frame_time;
      bool same = group[j] == group[jj];
      if ((history && same && before) || ((!same || last) && keep_last && before)) {
        tweendata.push_back(data[j]);
        tweengroup.push_back(group[j]);
        tweenframe.push_back(frame_time);
      }
      if (same && before == after) {
        double pos = (frame_time - time[j]) / (time[jj] - time[j]);
        pos = ease_pos(pos, easer);
        double interp = data[j] + (data[jj] - data[j]) * pos;
        tweendata.push_back(interp);
        tweengroup.push_back(group[j]);
        tweenframe.push_back(frame_time);
      }
    }
  }

  return cpp11::writable::data_frame({
    "data"_nm = tweendata,
    "group"_nm = tweengroup,
    "frame"_nm = tweenframe
  });
}
[[cpp11::register]]
cpp11::writable::data_frame colour_along_interpolator(cpp11::doubles_matrix<> data,
                                                      cpp11::integers group,
                                                      cpp11::doubles time,
                                                      bool history,
                                                      bool keep_last,
                                                      cpp11::doubles frames,
                                                      cpp11::strings ease) {
  cpp11::writable::doubles tweendata1;
  cpp11::writable::doubles tweendata2;
  cpp11::writable::doubles tweendata3;
  cpp11::writable::doubles tweendata4;
  cpp11::writable::integers tweengroup;
  cpp11::writable::integers tweenframe;
  std::string easer = ease[0];

  for (int i = 0; i < frames.size(); ++i) {
    double frame_time = frames[i];
    for (R_xlen_t j = 0; j < data.nrow(); ++j) {
      bool last = j == data.nrow() - 1;
      R_xlen_t jj = last ? j : j + 1;
      bool before = time[j] <= frame_time;
      bool after = time[jj] > frame_time;
      bool same = group[j] == group[jj];
      if ((history && same && before) || ((!same || last) && keep_last && before)) {
        tweendata1.push_back(data(j, 0));
        tweendata2.push_back(data(j, 1));
        tweendata3.push_back(data(j, 2));
        tweendata4.push_back(data(j, 3));
        tweengroup.push_back(group[j]);
        tweenframe.push_back(frame_time);
      }
      if (same && before == after) {
        double pos = (frame_time - time[j]) / (time[j + 1] - time[j]);
        pos = ease_pos(pos, easer);
        tweendata1.push_back(data(j, 0) + (data(j + 1, 0) - data(j, 0)) * pos);
        tweendata2.push_back(data(j, 1) + (data(j + 1, 1) - data(j, 1)) * pos);
        tweendata3.push_back(data(j, 2) + (data(j + 1, 2) - data(j, 2)) * pos);
        tweendata4.push_back(data(j, 3) + (data(j + 1, 3) - data(j, 3)) * pos);
        tweengroup.push_back(group[j]);
        tweenframe.push_back(frame_time);
      }
    }
  }

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
cpp11::writable::data_frame constant_along_interpolator(cpp11::strings data,
                                                        cpp11::integers group,
                                                        cpp11::doubles time,
                                                        bool history,
                                                        bool keep_last,
                                                        cpp11::doubles frames,
                                                        cpp11::strings ease) {
  cpp11::writable::strings tweendata;
  cpp11::writable::integers tweengroup;
  cpp11::writable::integers tweenframe;
  std::string easer = ease[0];

  for (int i = 0; i < frames.size(); ++i) {
    double frame_time = frames[i];
    for (R_len_t j = 0; j < data.size(); ++j) {
      bool last = j == data.size() - 1;
      R_xlen_t jj = last ? j : j + 1;
      bool before = time[j] <= frame_time;
      bool after = time[jj] > frame_time;
      bool same = group[j] == group[jj];
      if ((history && same && before) || ((!same || last) && keep_last && before)) {
        tweendata.push_back(data[j]);
        tweengroup.push_back(group[j]);
        tweenframe.push_back(frame_time);
      }
      if (same && before == after) {
        double pos = (frame_time - time[j]) / (time[j + 1] - time[j]);
        pos = ease_pos(pos, easer);
        if (pos < 0.5) {
          tweendata.push_back(data[j]);
        } else {
          tweendata.push_back(data[j + 1]);
        }
        tweengroup.push_back(group[j]);
        tweenframe.push_back(frame_time);
      }
    }
  }

  return cpp11::writable::data_frame({
    "data"_nm = tweendata,
    "group"_nm = tweengroup,
    "frame"_nm = tweenframe
  });
}

[[cpp11::register]]
cpp11::writable::data_frame list_along_interpolator(cpp11::list data,
                                                    cpp11::integers group,
                                                    cpp11::doubles time,
                                                    bool history,
                                                    bool keep_last,
                                                    cpp11::doubles frames,
                                                    cpp11::strings ease) {
  cpp11::writable::list tweendata;
  cpp11::writable::integers tweengroup;
  cpp11::writable::integers tweenframe;
  std::string easer = ease[0];

  for (int i = 0; i < frames.size(); ++i) {
    double frame_time = frames[i];
    for (R_len_t j = 0; j < data.size(); ++j) {
      bool last = j == data.size() - 1;
      R_xlen_t jj = last ? j : j + 1;
      bool before = time[j] <= frame_time;
      bool after = time[jj] > frame_time;
      bool same = group[j] == group[jj];
      if ((history && same && before) || ((!same || last) && keep_last && before)) {
        tweendata.push_back(data[j]);
        tweengroup.push_back(group[j]);
        tweenframe.push_back(frame_time);
      }
      if (same && before == after) {
        double pos = (frame_time - time[j]) / (time[j + 1] - time[j]);
        pos = ease_pos(pos, easer);
        if (pos < 0.5) {
          tweendata.push_back(data[j]);
        } else {
          tweendata.push_back(data[j + 1]);
        }
        tweengroup.push_back(group[j]);
        tweenframe.push_back(frame_time);
      }
    }
  }

  return cpp11::writable::data_frame({
    "data"_nm = tweendata,
    "group"_nm = tweengroup,
    "frame"_nm = tweenframe
  });
}
[[cpp11::register]]
cpp11::writable::data_frame numlist_along_interpolator(cpp11::list_of<cpp11::doubles> data,
                                                       cpp11::integers group,
                                                       cpp11::doubles time,
                                                       bool history,
                                                       bool keep_last,
                                                       cpp11::doubles frames,
                                                       cpp11::strings ease) {
  cpp11::writable::list tweendata;
  cpp11::writable::integers tweengroup;
  cpp11::writable::integers tweenframe;
  std::string easer = ease[0];

  for (int i = 0; i < frames.size(); ++i) {
    double frame_time = frames[i];
    for (R_xlen_t j = 0; j < data.size(); ++j) {
      bool last = j == data.size() - 1;
      R_xlen_t jj = last ? j : j + 1;
      bool before = time[j] <= frame_time;
      bool after = time[jj] > frame_time;
      bool same = group[j] == group[jj];
      if ((history && same && before) || ((!same || last) && keep_last && before)) {
        tweendata.push_back(data[j]);
        tweengroup.push_back(group[j]);
        tweenframe.push_back(frame_time);
      }
      if (same && before == after) {
        cpp11::doubles state_from_vec = data[j];
        cpp11::doubles state_to_vec = data[j + 1];
        state_from_vec = align_num_elem(state_from_vec, state_to_vec);
        state_to_vec = align_num_elem(state_to_vec, state_from_vec);
        double pos = (frame_time - time[j]) / (time[j + 1] - time[j]);
        pos = ease_pos(pos, easer);
        cpp11::writable::doubles state_vec(state_from_vec.size());
        for (R_xlen_t k = 0; k < state_from_vec.size(); ++k) {
          state_vec[k] = state_from_vec[k] + pos * (state_to_vec[k] - state_from_vec[k]);
        }
        tweendata.push_back(state_vec);
        tweengroup.push_back(group[j]);
        tweenframe.push_back(frame_time);
      }
    }
  }

  return cpp11::writable::data_frame({
    "data"_nm = tweendata,
    "group"_nm = tweengroup,
    "frame"_nm = tweenframe
  });
}
[[cpp11::register]]
cpp11::writable::data_frame phase_along_interpolator(cpp11::integers group,
                                                     cpp11::doubles time,
                                                     bool history,
                                                     bool keep_last,
                                                     cpp11::doubles frames) {
  cpp11::writable::strings tweendata;
  cpp11::writable::integers tweengroup;
  cpp11::writable::integers tweenframe;

  for (int i = 0; i < frames.size(); ++i) {
    double frame_time = frames[i];
    for (R_xlen_t j = 0; j < group.size(); ++j) {
      bool last = j == group.size() - 1;
      R_xlen_t jj = last ? j : j + 1;
      bool before = time[j] <= frame_time;
      bool after = time[jj] > frame_time;
      bool same = group[j] == group[jj];
      if ((history && same && before) || ((!same || last) && keep_last && before)) {
        tweendata.push_back("raw");
        tweengroup.push_back(group[j]);
        tweenframe.push_back(frame_time);
      }
      if (same && before == after) {
        tweendata.push_back("transition");
        tweengroup.push_back(group[j]);
        tweenframe.push_back(frame_time);
      }
    }
  }

  return cpp11::writable::data_frame({
    "data"_nm = tweendata,
    "group"_nm = tweengroup,
    "frame"_nm = tweenframe
  });
}
