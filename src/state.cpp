#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/strings.hpp>
#include <cpp11/list.hpp>
#include <cpp11/list_of.hpp>
#include <cpp11/data_frame.hpp>
#include <cpp11/matrix.hpp>

#include <numeric>
#include <algorithm>

#include "utils.h"

[[cpp11::register]]
cpp11::doubles numeric_state_interpolator(cpp11::list_of<cpp11::doubles> data, cpp11::data_frame states) {
  cpp11::integers state_index = states["state"];
  cpp11::integers nframes_per_state = states["nframes"];
  cpp11::strings easer = states["ease"];
  R_xlen_t nelements = data[0].size();
  R_xlen_t nstates = states.nrow();
  int nframes = std::accumulate(nframes_per_state.begin(), nframes_per_state.end(), 0);
  int frame = 0;
  cpp11::writable::doubles res(nelements * nframes);

  for (R_xlen_t state = 0; state < nstates; ++state) {
    if (easer[state] == "constant") {
      cpp11::doubles state_from = data[state_index[state]];
      for (int currentframe = 0; currentframe < nframes_per_state[state]; ++currentframe) {
        R_xlen_t res_index = (frame + currentframe) * nelements;
        for (R_xlen_t element = 0; element < nelements; ++element) {
          res[res_index] = state_from[element];
          ++res_index;
        }
      }
    } else {
      std::vector<double> ease_points = ease_seq(easer[state], nframes_per_state[state]);
      cpp11::doubles state_from = data[state_index[state]];
      cpp11::doubles state_to = data[state_index[state] + 1];
      for (R_xlen_t element = 0; element < nelements; ++element) {
        for (int currentframe = 0; currentframe < nframes_per_state[state]; ++currentframe) {
          R_xlen_t res_index = (frame + currentframe) * nelements + element;
          res[res_index] = state_from[element] + ease_points[currentframe] * (state_to[element] - state_from[element]);
        }
      }
    }
    frame += nframes_per_state[state];
  }

  return res;
}

[[cpp11::register]]
cpp11::doubles_matrix<> colour_state_interpolator(cpp11::list_of<cpp11::doubles_matrix<>> data, cpp11::data_frame states) {
  cpp11::integers state_index = states["state"];
  cpp11::integers nframes_per_state = states["nframes"];
  cpp11::strings easer = states["ease"];
  R_xlen_t nelements = data[0].nrow();
  R_xlen_t nstates = states.nrow();
  int nframes = std::accumulate(nframes_per_state.begin(), nframes_per_state.end(), 0);
  int frame = 0;
  cpp11::writable::doubles_matrix<> res(nelements * nframes, 4);

  for (R_xlen_t state = 0; state < nstates; ++state) {
    if (easer[state] == "constant") {
      cpp11::doubles_matrix<> state_from = data[state_index[state]];
      for (int currentframe = 0; currentframe < nframes_per_state[state]; ++currentframe) {
        R_xlen_t res_index = (frame + currentframe) * nelements;
        for (R_xlen_t element = 0; element < nelements; ++element) {
          res(res_index, 0) = state_from(element, 0);
          res(res_index, 1) = state_from(element, 1);
          res(res_index, 2) = state_from(element, 2);
          res(res_index, 3) = state_from(element, 3);
          ++res_index;
        }
      }
    } else {
      std::vector<double> ease_points = ease_seq(easer[state], nframes_per_state[state]);
      cpp11::doubles_matrix<> state_from = data[state_index[state]];
      cpp11::doubles_matrix<> state_to = data[state_index[state] + 1];
      for (R_xlen_t element = 0; element < nelements; ++element) {
        for (int currentframe = 0; currentframe < nframes_per_state[state]; ++currentframe) {
          R_xlen_t res_index = (frame + currentframe) * nelements + element;
          res(res_index, 0) = state_from(element, 0) + ease_points[currentframe] * (state_to(element, 0) - state_from(element, 0));
          res(res_index, 1) = state_from(element, 1) + ease_points[currentframe] * (state_to(element, 1) - state_from(element, 1));
          res(res_index, 2) = state_from(element, 2) + ease_points[currentframe] * (state_to(element, 2) - state_from(element, 2));
          res(res_index, 3) = state_from(element, 3) + ease_points[currentframe] * (state_to(element, 3) - state_from(element, 3));
        }
      }
    }
    frame += nframes_per_state[state];
  }

  return res;
}

[[cpp11::register]]
cpp11::strings constant_state_interpolator(cpp11::list_of<cpp11::strings> data, cpp11::data_frame states) {
  cpp11::integers state_index = states["state"];
  cpp11::integers nframes_per_state = states["nframes"];
  cpp11::strings easer = states["ease"];
  R_xlen_t nelements = data[0].size();
  R_xlen_t nstates = states.nrow();
  int nframes = std::accumulate(nframes_per_state.begin(), nframes_per_state.end(), 0);
  int frame = 0;
  cpp11::writable::strings res(nelements * nframes);

  for (R_xlen_t state = 0; state < nstates; ++state) {
    if (easer[state] == "constant") {
      cpp11::strings state_from = data[state_index[state]];
      for (int currentframe = 0; currentframe < nframes_per_state[state]; ++currentframe) {
        R_xlen_t res_index = (frame + currentframe) * nelements;
        for (R_xlen_t element = 0; element < nelements; ++element) {
          res[res_index] = state_from[element];
          ++res_index;
        }
      }
    } else {
      std::vector<double> ease_points = ease_seq(easer[state], nframes_per_state[state]);
      cpp11::strings state_from = data[state_index[state]];
      cpp11::strings state_to = data[state_index[state] + 1];
      for (R_xlen_t element = 0; element < nelements; ++element) {
        for (int currentframe = 0; currentframe < nframes_per_state[state]; ++currentframe) {
          R_xlen_t res_index = (frame + currentframe) * nelements + element;
          if (ease_points[currentframe] < 0.5) {
            res[res_index] = state_from[element];
          } else {
            res[res_index] = state_to[element];
          }
        }
      }
    }
    frame += nframes_per_state[state];
  }

  return res;
}
[[cpp11::register]]
cpp11::list list_state_interpolator(cpp11::list_of<cpp11::list> data, cpp11::data_frame states) {
  cpp11::integers state_index = states["state"];
  cpp11::integers nframes_per_state = states["nframes"];
  cpp11::strings easer = states["ease"];
  R_xlen_t nelements = data[0].size();
  R_xlen_t nstates = states.nrow();
  int nframes = std::accumulate(nframes_per_state.begin(), nframes_per_state.end(), 0);
  int frame = 0;
  cpp11::writable::list res(nelements * nframes);

  for (R_xlen_t state = 0; state < nstates; ++state) {
    if (easer[state] == "constant") {
      cpp11::list state_from = data[state_index[state]];
      for (int currentframe = 0; currentframe < nframes_per_state[state]; ++currentframe) {
        R_xlen_t res_index = (frame + currentframe) * nelements;
        for (R_xlen_t element = 0; element < nelements; ++element) {
          res[res_index] = state_from[element];
          ++res_index;
        }
      }
    } else {
      std::vector<double> ease_points = ease_seq(easer[state], nframes_per_state[state]);
      cpp11::list state_from = data[state_index[state]];
      cpp11::list state_to = data[state_index[state] + 1];
      for (R_xlen_t element = 0; element < nelements; ++element) {
        for (int currentframe = 0; currentframe < nframes_per_state[state]; ++currentframe) {
          R_xlen_t res_index = (frame + currentframe) * nelements + element;
          if (ease_points[currentframe] < 0.5) {
            res[res_index] = state_from[element];
          } else {
            res[res_index] = state_to[element];
          }
        }
      }
    }
    frame += nframes_per_state[state];
  }

  return res;
}

[[cpp11::register]]
cpp11::list numlist_state_interpolator(cpp11::list_of<cpp11::list> data, cpp11::data_frame states) {
  cpp11::integers state_index = states["state"];
  cpp11::integers nframes_per_state = states["nframes"];
  cpp11::strings easer = states["ease"];
  R_xlen_t nelements = data[0].size();
  R_xlen_t nstates = states.nrow();
  int nframes = std::accumulate(nframes_per_state.begin(), nframes_per_state.end(), 0);
  int frame = 0;
  cpp11::writable::list res(nelements * nframes);

  for (R_xlen_t state = 0; state < nstates; ++state) {
    if (easer[state] == "constant") {
      cpp11::list state_from = data[state_index[state]];
      for (int currentframe = 0; currentframe < nframes_per_state[state]; ++currentframe) {
        R_xlen_t res_index = (frame + currentframe) * nelements;
        for (R_xlen_t element = 0; element < nelements; ++element) {
          res[res_index] = state_from[element];
          ++res_index;
        }
      }
    } else {
      std::vector<double> ease_points = ease_seq(easer[state], nframes_per_state[state]);
      cpp11::list state_from = data[state_index[state]];
      cpp11::list state_to = data[state_index[state] + 1];
      for (R_xlen_t element = 0; element < nelements; ++element) {
        cpp11::doubles state_from_vec = state_from[element];
        cpp11::doubles state_to_vec = state_to[element];
        state_from_vec = align_num_elem(state_from_vec, state_to_vec);
        state_to_vec = align_num_elem(state_to_vec, state_from_vec);
        for (int currentframe = 0; currentframe < nframes_per_state[state]; ++currentframe) {
          R_xlen_t res_index = (frame + currentframe) * nelements + element;
          cpp11::writable::doubles state_vec(state_from_vec.size());
          for (R_xlen_t i = 0; i < state_from_vec.size(); ++i) {
            state_vec[i] = state_from_vec[i] + ease_points[currentframe] * (state_to_vec[i] - state_from_vec[i]);
          }
          res[res_index] = state_vec;
        }
      }
    }
    frame += nframes_per_state[state];
  }

  return res;
}

[[cpp11::register]]
cpp11::strings phase_state_interpolator(cpp11::list_of<cpp11::strings> data, cpp11::data_frame states) {
  cpp11::integers state_index = states["state"];
  cpp11::integers nframes_per_state = states["nframes"];
  cpp11::strings easer = states["ease"];
  R_xlen_t nelements = data[0].size();
  R_xlen_t nstates = states.nrow();
  int nframes = std::accumulate(nframes_per_state.begin(), nframes_per_state.end(), 0);
  int frame = 0;
  cpp11::writable::strings res(nelements * nframes);

  for (R_xlen_t state = 0; state < nstates; ++state) {
    if (easer[state] == "constant") {
      cpp11::strings state_from = data[state_index[state]];
      for (int currentframe = 0; currentframe < nframes_per_state[state]; ++currentframe) {
        std::string type = currentframe == nframes_per_state[state] - 1 ? "raw" : "static";
        R_xlen_t res_index = (frame + currentframe) * nelements;
        for (R_xlen_t element = 0; element < nelements; ++element) {
          res[res_index] = type;
          ++res_index;
        }
      }
    } else {
      std::vector<double> ease_points = ease_seq(easer[state], nframes_per_state[state]);
      cpp11::strings state_from = data[state_index[state]];
      cpp11::strings state_to = data[state_index[state] + 1];
      for (R_xlen_t element = 0; element < nelements; ++element) {
        std::string type = state_from[element] == "enter" ? "enter" : state_to[element] == "exit" ? "exit" : "transition";
        for (int currentframe = 0; currentframe < nframes_per_state[state]; ++currentframe) {
          R_xlen_t res_index = (frame + currentframe) * nelements + element;
          res[res_index] = type;
        }
      }
    }
    frame += nframes_per_state[state];
  }

  return res;
}
