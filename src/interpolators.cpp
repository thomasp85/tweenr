#include <Rcpp.h>
#include "easing.h"

using namespace Rcpp;

enum easeEnum {
  linear,
  quadratic_in,
  quadratic_out,
  quadratic_in_out,
  cubic_in,
  cubic_out,
  cubic_in_out,
  quartic_in,
  quartic_out,
  quartic_in_out,
  quintic_in,
  quintic_out,
  quintic_in_out,
  sine_in,
  sine_out,
  sine_in_out,
  circular_in,
  circular_out,
  circular_in_out,
  exponential_in,
  exponential_out,
  exponential_in_out,
  elastic_in,
  elastic_out,
  elastic_in_out,
  back_in,
  back_out,
  back_in_out,
  bounce_in,
  bounce_out,
  bounce_in_out,
  UNKNOWN
};
easeEnum hashEase(std::string ease) {
  if (ease == "linear") return linear;
  if (ease == "quadratic-in") return quadratic_in;
  if (ease == "quadratic-out") return quadratic_out;
  if (ease == "quadratic-in-out") return quadratic_in_out;
  if (ease == "cubic-in") return cubic_in;
  if (ease == "cubic-out") return cubic_out;
  if (ease == "cubic-in-out") return cubic_in_out;
  if (ease == "quartic-in") return quartic_in;
  if (ease == "quartic-out") return quartic_out;
  if (ease == "quartic-in-out") return quartic_in_out;
  if (ease == "quintic-in") return quintic_in;
  if (ease == "quintic-out") return quintic_out;
  if (ease == "quintic-in-out") return quintic_in_out;
  if (ease == "sine-in") return sine_in;
  if (ease == "sine-out") return sine_out;
  if (ease == "sine-in-out") return sine_in_out;
  if (ease == "circular-in") return circular_in;
  if (ease == "circular-out") return circular_out;
  if (ease == "circular-in-out") return circular_in_out;
  if (ease == "exponential-in") return exponential_in;
  if (ease == "exponential-out") return exponential_out;
  if (ease == "exponential-in-out") return exponential_in_out;
  if (ease == "elastic-in") return elastic_in;
  if (ease == "elastic-out") return elastic_out;
  if (ease == "elastic-in-out") return elastic_in_out;
  if (ease == "back-in") return back_in;
  if (ease == "back-out") return back_out;
  if (ease == "back-in-out") return back_in_out;
  if (ease == "bounce-in") return bounce_in;
  if (ease == "bounce-out") return bounce_out;
  if (ease == "bounce-in-out") return bounce_in_out;
  return UNKNOWN;
}

std::vector<double> easeSeq(std::string easer, int length) {
  std::vector<double> res(length);
  double p;
  // Just linear for now
  for(int i = 0; i < length; ++i) {
    p = double(i) / length;
    switch (hashEase(easer)) {
    case linear:
      res[i] = LinearInterpolation(p);
      break;
    case quadratic_in:
      res[i] = QuadraticEaseIn(p);
      break;
    case quadratic_out:
      res[i] = QuadraticEaseOut(p);
      break;
    case quadratic_in_out:
      res[i] = QuadraticEaseInOut(p);
      break;
    case cubic_in:
      res[i] = CubicEaseIn(p);
      break;
    case cubic_out:
      res[i] = CubicEaseOut(p);
      break;
    case cubic_in_out:
      res[i] = CubicEaseInOut(p);
      break;
    case quartic_in:
      res[i] = QuarticEaseIn(p);
      break;
    case quartic_out:
      res[i] = QuarticEaseOut(p);
      break;
    case quartic_in_out:
      res[i] = QuarticEaseInOut(p);
      break;
    case quintic_in:
      res[i] = QuinticEaseIn(p);
      break;
    case quintic_out:
      res[i] = QuinticEaseOut(p);
      break;
    case quintic_in_out:
      res[i] = QuinticEaseInOut(p);
      break;
    case sine_in:
      res[i] = SineEaseIn(p);
      break;
    case sine_out:
      res[i] = SineEaseOut(p);
      break;
    case sine_in_out:
      res[i] = SineEaseInOut(p);
      break;
    case circular_in:
      res[i] = CircularEaseIn(p);
      break;
    case circular_out:
      res[i] = CircularEaseOut(p);
      break;
    case circular_in_out:
      res[i] = CircularEaseInOut(p);
      break;
    case exponential_in:
      res[i] = ExponentialEaseIn(p);
      break;
    case exponential_out:
      res[i] = ExponentialEaseOut(p);
      break;
    case exponential_in_out:
      res[i] = ExponentialEaseInOut(p);
      break;
    case elastic_in:
      res[i] = ElasticEaseIn(p);
      break;
    case elastic_out:
      res[i] = ElasticEaseOut(p);
      break;
    case elastic_in_out:
      res[i] = ElasticEaseInOut(p);
      break;
    case back_in:
      res[i] = BackEaseIn(p);
      break;
    case back_out:
      res[i] = BackEaseOut(p);
      break;
    case back_in_out:
      res[i] = BackEaseInOut(p);
      break;
    case bounce_in:
      res[i] = BounceEaseIn(p);
      break;
    case bounce_out:
      res[i] = BounceEaseOut(p);
      break;
    case bounce_in_out:
      res[i] = BounceEaseInOut(p);
      break;
    case UNKNOWN:
      stop("Unknown easing function");
    }
  }
  return res;
}
double easePos(double p, std::string easer) {
  double p_new;
  switch (hashEase(easer)) {
  case linear:
    p_new = LinearInterpolation(p);
    break;
  case quadratic_in:
    p_new = QuadraticEaseIn(p);
    break;
  case quadratic_out:
    p_new = QuadraticEaseOut(p);
    break;
  case quadratic_in_out:
    p_new = QuadraticEaseInOut(p);
    break;
  case cubic_in:
    p_new = CubicEaseIn(p);
    break;
  case cubic_out:
    p_new = CubicEaseOut(p);
    break;
  case cubic_in_out:
    p_new = CubicEaseInOut(p);
    break;
  case quartic_in:
    p_new = QuarticEaseIn(p);
    break;
  case quartic_out:
    p_new = QuarticEaseOut(p);
    break;
  case quartic_in_out:
    p_new = QuarticEaseInOut(p);
    break;
  case quintic_in:
    p_new = QuinticEaseIn(p);
    break;
  case quintic_out:
    p_new = QuinticEaseOut(p);
    break;
  case quintic_in_out:
    p_new = QuinticEaseInOut(p);
    break;
  case sine_in:
    p_new = SineEaseIn(p);
    break;
  case sine_out:
    p_new = SineEaseOut(p);
    break;
  case sine_in_out:
    p_new = SineEaseInOut(p);
    break;
  case circular_in:
    p_new = CircularEaseIn(p);
    break;
  case circular_out:
    p_new = CircularEaseOut(p);
    break;
  case circular_in_out:
    p_new = CircularEaseInOut(p);
    break;
  case exponential_in:
    p_new = ExponentialEaseIn(p);
    break;
  case exponential_out:
    p_new = ExponentialEaseOut(p);
    break;
  case exponential_in_out:
    p_new = ExponentialEaseInOut(p);
    break;
  case elastic_in:
    p_new = ElasticEaseIn(p);
    break;
  case elastic_out:
    p_new = ElasticEaseOut(p);
    break;
  case elastic_in_out:
    p_new = ElasticEaseInOut(p);
    break;
  case back_in:
    p_new = BackEaseIn(p);
    break;
  case back_out:
    p_new = BackEaseOut(p);
    break;
  case back_in_out:
    p_new = BackEaseInOut(p);
    break;
  case bounce_in:
    p_new = BounceEaseIn(p);
    break;
  case bounce_out:
    p_new = BounceEaseOut(p);
    break;
  case bounce_in_out:
    p_new = BounceEaseInOut(p);
    break;
  case UNKNOWN:
    stop("Unknown easing function");
  }
  return p_new;
}


//[[Rcpp::export]]
NumericVector numeric_state_interpolator(List data, DataFrame states) {
  IntegerVector state_index = states("state");
  NumericVector nframes_per_state = states("nframes");
  std::vector<std::string> easer = states("ease");
  int nelements = as<NumericVector>(data(0)).size();
  int nstates = states.nrows();
  int nframes = sum(nframes_per_state);
  int frame = 0;
  int state, element, currentframe, res_index;
  NumericVector res(nelements * nframes);

  for (state = 0; state < nstates; ++state) {
    if (easer[state] == "constant") {
      NumericVector state_from = data(state_index(state));
      for (currentframe = 0; currentframe < nframes_per_state(state); ++currentframe) {
        res_index = (frame + currentframe) * nelements;
        for (element = 0; element < nelements; ++element) {
          res[res_index] = state_from[element];
          ++res_index;
        }
      }
    } else {
      std::vector<double> ease_points = easeSeq(easer[state], nframes_per_state(state));
      NumericVector state_from = data(state_index(state));
      NumericVector state_to = data(state_index(state) + 1);
      for (element = 0; element < nelements; ++element) {
        for (currentframe = 0; currentframe < nframes_per_state(state); ++currentframe) {
          res_index = (frame + currentframe) * nelements + element;
          res[res_index] = state_from[element] + ease_points[currentframe] * (state_to[element] - state_from[element]);
        }
      }
    }
    frame += nframes_per_state(state);
  }

  return res;
}

//[[Rcpp::export]]
NumericMatrix colour_state_interpolator(List data, DataFrame states) {
  IntegerVector state_index = states("state");
  NumericVector nframes_per_state = states("nframes");
  std::vector<std::string> easer = states("ease");
  int nelements = as<NumericMatrix>(data(0)).nrow();
  int nstates = states.nrows();
  int nframes = sum(nframes_per_state);
  int frame = 0;
  int state, element, currentframe, res_index;
  NumericMatrix res(nelements * nframes, 4);

  for (state = 0; state < nstates; ++state) {
    if (easer[state] == "constant") {
      NumericMatrix state_from = data(state_index(state));
      for (currentframe = 0; currentframe < nframes_per_state(state); ++currentframe) {
        res_index = (frame + currentframe) * nelements;
        for (element = 0; element < nelements; ++element) {
          res(res_index, 0) = state_from(element, 0);
          res(res_index, 1) = state_from(element, 1);
          res(res_index, 2) = state_from(element, 2);
          res(res_index, 3) = state_from(element, 3);
          ++res_index;
        }
      }
    } else {
      std::vector<double> ease_points = easeSeq(easer[state], nframes_per_state(state));
      NumericMatrix state_from = data(state_index(state));
      NumericMatrix state_to = data(state_index(state) + 1);
      for (element = 0; element < nelements; ++element) {
        for (currentframe = 0; currentframe < nframes_per_state(state); ++currentframe) {
          res_index = (frame + currentframe) * nelements + element;
          res(res_index, 0) = state_from(element, 0) + ease_points[currentframe] * (state_to(element, 0) - state_from(element, 0));
          res(res_index, 1) = state_from(element, 1) + ease_points[currentframe] * (state_to(element, 1) - state_from(element, 1));
          res(res_index, 2) = state_from(element, 2) + ease_points[currentframe] * (state_to(element, 2) - state_from(element, 2));
          res(res_index, 3) = state_from(element, 3) + ease_points[currentframe] * (state_to(element, 3) - state_from(element, 3));
        }
      }
    }
    frame += nframes_per_state(state);
  }

  return res;
}

//[[Rcpp::export]]
CharacterVector constant_state_interpolator(List data, DataFrame states) {
  IntegerVector state_index = states("state");
  NumericVector nframes_per_state = states("nframes");
  std::vector<std::string> easer = states("ease");
  int nelements = as<CharacterVector>(data(0)).size();
  int nstates = states.nrows();
  int nframes = sum(nframes_per_state);
  int frame = 0;
  int state, element, currentframe, res_index;
  CharacterVector res(nelements * nframes);

  for (state = 0; state < nstates; ++state) {
    if (easer[state] == "constant") {
      CharacterVector state_from = data(state_index(state));
      for (currentframe = 0; currentframe < nframes_per_state(state); ++currentframe) {
        res_index = (frame + currentframe) * nelements;
        for (element = 0; element < nelements; ++element) {
          res[res_index] = state_from[element];
          ++res_index;
        }
      }
    } else {
      std::vector<double> ease_points = easeSeq(easer[state], nframes_per_state(state));
      CharacterVector state_from = data(state_index(state));
      CharacterVector state_to = data(state_index(state) + 1);
      for (element = 0; element < nelements; ++element) {
        for (currentframe = 0; currentframe < nframes_per_state(state); ++currentframe) {
          res_index = (frame + currentframe) * nelements + element;
          if (ease_points[currentframe] < 0.5) {
            res[res_index] = state_from[element];
          } else {
            res[res_index] = state_to[element];
          }
        }
      }
    }
    frame += nframes_per_state(state);
  }

  return res;
}
//[[Rcpp::export]]
List list_state_interpolator(List data, DataFrame states) {
  IntegerVector state_index = states("state");
  NumericVector nframes_per_state = states("nframes");
  std::vector<std::string> easer = states("ease");
  int nelements = as<List>(data(0)).size();
  int nstates = states.nrows();
  int nframes = sum(nframes_per_state);
  int frame = 0;
  int state, element, currentframe, res_index;
  List res(nelements * nframes);

  for (state = 0; state < nstates; ++state) {
    if (easer[state] == "constant") {
      List state_from = data(state_index(state));
      for (currentframe = 0; currentframe < nframes_per_state(state); ++currentframe) {
        res_index = (frame + currentframe) * nelements;
        for (element = 0; element < nelements; ++element) {
          res[res_index] = state_from[element];
          ++res_index;
        }
      }
    } else {
      std::vector<double> ease_points = easeSeq(easer[state], nframes_per_state(state));
      List state_from = data(state_index(state));
      List state_to = data(state_index(state) + 1);
      for (element = 0; element < nelements; ++element) {
        for (currentframe = 0; currentframe < nframes_per_state(state); ++currentframe) {
          res_index = (frame + currentframe) * nelements + element;
          if (ease_points[currentframe] < 0.5) {
            res[res_index] = state_from[element];
          } else {
            res[res_index] = state_to[element];
          }
        }
      }
    }
    frame += nframes_per_state(state);
  }

  return res;
}
NumericVector align_num_elem(NumericVector from, NumericVector to) {
  NumericVector res;
  if (from.size() < to.size()) {
    if (from.size() == 0) {
      res = NumericVector(to.size(), mean(to));
    } else {
      res = NumericVector(to.size());
      for (int i = 0; i < res.size(); ++i) {
        res[i] = from[i % from.size()];
      }
    }
  } else {
    res = from;
  }
  return res;
}
//[[Rcpp::export]]
List numlist_state_interpolator(List data, DataFrame states) {
  IntegerVector state_index = states("state");
  NumericVector nframes_per_state = states("nframes");
  std::vector<std::string> easer = states("ease");
  int nelements = as<List>(data(0)).size();
  int nstates = states.nrows();
  int nframes = sum(nframes_per_state);
  int frame = 0;
  int state, element, currentframe, res_index;
  List res(nelements * nframes);

  for (state = 0; state < nstates; ++state) {
    if (easer[state] == "constant") {
      List state_from = data(state_index(state));
      for (currentframe = 0; currentframe < nframes_per_state(state); ++currentframe) {
        res_index = (frame + currentframe) * nelements;
        for (element = 0; element < nelements; ++element) {
          res[res_index] = state_from[element];
          ++res_index;
        }
      }
    } else {
      std::vector<double> ease_points = easeSeq(easer[state], nframes_per_state(state));
      List state_from = data(state_index(state));
      List state_to = data(state_index(state) + 1);
      for (element = 0; element < nelements; ++element) {
        NumericVector state_from_vec = state_from[element];
        NumericVector state_to_vec = state_to[element];
        state_from_vec = align_num_elem(state_from_vec, state_to_vec);
        state_to_vec = align_num_elem(state_to_vec, state_from_vec);
        for (currentframe = 0; currentframe < nframes_per_state(state); ++currentframe) {
          res_index = (frame + currentframe) * nelements + element;
          NumericVector state_vec = state_from_vec + ease_points[currentframe] * (state_to_vec - state_from_vec);
          res[res_index] = state_vec;
        }
      }
    }
    frame += nframes_per_state(state);
  }

  return res;
}

//[[Rcpp::export]]
CharacterVector phase_state_interpolator(List data, DataFrame states) {
  IntegerVector state_index = states("state");
  NumericVector nframes_per_state = states("nframes");
  std::vector<std::string> easer = states("ease");
  int nelements = as<CharacterVector>(data(0)).size();
  int nstates = states.nrows();
  int nframes = sum(nframes_per_state);
  int frame = 0;
  int state, element, currentframe, res_index;
  CharacterVector res(nelements * nframes);

  for (state = 0; state < nstates; ++state) {
    if (easer[state] == "constant") {
      CharacterVector state_from = data(state_index(state));
      for (currentframe = 0; currentframe < nframes_per_state(state); ++currentframe) {
        std::string type = currentframe == nframes_per_state(state) - 1 ? "raw" : "static";
        res_index = (frame + currentframe) * nelements;
        for (element = 0; element < nelements; ++element) {
          res[res_index] = type;
          ++res_index;
        }
      }
    } else {
      std::vector<double> ease_points = easeSeq(easer[state], nframes_per_state(state));
      CharacterVector state_from = data(state_index(state));
      CharacterVector state_to = data(state_index(state) + 1);
      for (element = 0; element < nelements; ++element) {
        std::string type = state_from[element] == "enter" ? "enter" : state_to[element] == "exit" ? "exit" : "transition";
        for (currentframe = 0; currentframe < nframes_per_state(state); ++currentframe) {
          res_index = (frame + currentframe) * nelements + element;
          res[res_index] = type;
        }
      }
    }
    frame += nframes_per_state(state);
  }

  return res;
}

//[[Rcpp::export]]
DataFrame numeric_element_interpolator(NumericVector data, CharacterVector group, IntegerVector frame, CharacterVector ease) {
  std::deque<double> tweendata;
  std::deque<std::string> tweengroup;
  std::deque<int> tweenframe;
  int i, j, nframes;
  std::string groupString;
  std::string currentGroup = as<std::string>(group[0]);

  for (i = 1; i < data.size(); ++i) {
    groupString = as<std::string>(group[i]);
    if (currentGroup.compare(groupString) == 0) {
      nframes = frame[i] - frame[i-1];
      std::vector<double> ease_points = easeSeq(as<std::string>(ease[i-1]), nframes);
      for (j = 0; j < ease_points.size(); ++j) {
        tweendata.push_back(data[i - 1] + ease_points[j] * (data[i] - data[i - 1]));
        tweengroup.push_back(groupString);
        tweenframe.push_back(j + frame[i-1]);
      }
    } else {
      tweendata.push_back(data[i - 1]);
      tweengroup.push_back(currentGroup);
      tweenframe.push_back(frame[i-1]);
      currentGroup = groupString;
    }
  }
  tweendata.push_back(data[i - 1]);
  tweengroup.push_back(currentGroup);
  tweenframe.push_back(frame[i-1]);

  return DataFrame::create(
    Named("data") = wrap(tweendata),
    Named("group") = wrap(tweengroup),
    Named("frame") = wrap(tweenframe),
    Named("stringsAsFactors") = false
  );
}

//[[Rcpp::export]]
DataFrame colour_element_interpolator(NumericMatrix data, CharacterVector group, IntegerVector frame, CharacterVector ease) {
  std::deque<double> tweendata1;
  std::deque<double> tweendata2;
  std::deque<double> tweendata3;
  std::deque<double> tweendata4;
  std::deque<std::string> tweengroup;
  std::deque<int> tweenframe;
  int i, j, nframes;
  std::string groupString;
  std::string currentGroup = as<std::string>(group[0]);

  for (i = 1; i < data.nrow(); ++i) {
    groupString = as<std::string>(group[i]);
    if (currentGroup.compare(groupString) == 0) {
      nframes = frame[i] - frame[i-1];
      std::vector<double> ease_points = easeSeq(as<std::string>(ease[i-1]), nframes);
      for (j = 0; j < ease_points.size(); ++j) {
        tweendata1.push_back(data(i - 1, 0) + ease_points[j] * (data(i, 0) - data(i - 1, 0)));
        tweendata2.push_back(data(i - 1, 1) + ease_points[j] * (data(i, 1) - data(i - 1, 1)));
        tweendata3.push_back(data(i - 1, 2) + ease_points[j] * (data(i, 2) - data(i - 1, 2)));
        tweendata4.push_back(data(i - 1, 3) + ease_points[j] * (data(i, 3) - data(i - 1, 3)));
        tweengroup.push_back(groupString);
        tweenframe.push_back(j + frame[i-1]);
      }
    } else {
      tweendata1.push_back(data(i - 1, 0));
      tweendata2.push_back(data(i - 1, 1));
      tweendata3.push_back(data(i - 1, 2));
      tweendata4.push_back(data(i - 1, 3));
      tweengroup.push_back(currentGroup);
      tweenframe.push_back(frame[i-1]);
      currentGroup = groupString;
    }
  }
  tweendata1.push_back(data(i - 1, 0));
  tweendata2.push_back(data(i - 1, 1));
  tweendata3.push_back(data(i - 1, 2));
  tweendata4.push_back(data(i - 1, 3));
  tweengroup.push_back(currentGroup);
  tweenframe.push_back(frame[i-1]);

  return DataFrame::create(
    Named("data1") = wrap(tweendata1),
    Named("data2") = wrap(tweendata2),
    Named("data3") = wrap(tweendata3),
    Named("data4") = wrap(tweendata4),
    Named("group") = wrap(tweengroup),
    Named("frame") = wrap(tweenframe),
    Named("stringsAsFactors") = false
  );
}

//[[Rcpp::export]]
DataFrame constant_element_interpolator(CharacterVector data, CharacterVector group, IntegerVector frame, CharacterVector ease) {
  std::deque<std::string> tweendata;
  std::deque<std::string> tweengroup;
  std::deque<int> tweenframe;
  int i, j, nframes;
  std::string groupString;
  std::string currentGroup = as<std::string>(group[0]);

  for (i = 1; i < data.size(); ++i) {
    groupString = as<std::string>(group[i]);
    if (currentGroup.compare(groupString) == 0) {
      nframes = frame[i] - frame[i-1];
      std::vector<double> ease_points = easeSeq(as<std::string>(ease[i-1]), nframes);
      for (j = 0; j < ease_points.size(); ++j) {
        if (ease_points[j] < 0.5) {
          tweendata.push_back(as<std::string>(data[i - 1]));
        } else {
          tweendata.push_back(as<std::string>(data[i]));
        }
        tweengroup.push_back(groupString);
        tweenframe.push_back(j + frame[i-1]);
      }
    } else {
      tweendata.push_back(as<std::string>(data[i - 1]));
      tweengroup.push_back(currentGroup);
      tweenframe.push_back(frame[i-1]);
      currentGroup = groupString;
    }

  }
  tweendata.push_back(as<std::string>(data[i - 1]));
  tweengroup.push_back(currentGroup);
  tweenframe.push_back(frame[i-1]);

  return DataFrame::create(
    Named("data") = wrap(tweendata),
    Named("group") = wrap(tweengroup),
    Named("frame") = wrap(tweenframe),
    Named("stringsAsFactors") = false
  );
}

//[[Rcpp::export]]
List list_element_interpolator(List data, CharacterVector group, IntegerVector frame, CharacterVector ease) {
  std::deque<SEXP> tweendata;
  std::deque<std::string> tweengroup;
  std::deque<int> tweenframe;
  int i, j, nframes;
  std::string groupString;
  std::string currentGroup = as<std::string>(group[0]);

  for (i = 1; i < data.size(); ++i) {
    groupString = as<std::string>(group[i]);
    if (currentGroup.compare(groupString) == 0) {
      nframes = frame[i] - frame[i-1];
      std::vector<double> ease_points = easeSeq(as<std::string>(ease[i-1]), nframes);
      for (j = 0; j < ease_points.size(); ++j) {
        if (ease_points[j] < 0.5) {
          tweendata.push_back(data[i - 1]);
        } else {
          tweendata.push_back(data[i]);
        }
        tweengroup.push_back(groupString);
        tweenframe.push_back(j + frame[i-1]);
      }
    } else {
      tweendata.push_back(data[i - 1]);
      tweengroup.push_back(currentGroup);
      tweenframe.push_back(frame[i-1]);
      currentGroup = groupString;
    }

  }
  tweendata.push_back(data[i - 1]);
  tweengroup.push_back(currentGroup);
  tweenframe.push_back(frame[i-1]);
  List tweendata_list = wrap(tweendata);
  IntegerVector frame_vec = wrap(tweenframe);
  CharacterVector group_vec = wrap(tweengroup);
  List res = List::create(
    Named("data") = tweendata_list,
    Named("group") = group_vec,
    Named("frame") = frame_vec
  );
  res.attr("class") = "data.frame";
  res.attr("row.names") = seq_along(frame_vec);
  return res;
}

//[[Rcpp::export]]
List numlist_element_interpolator(List data, CharacterVector group, IntegerVector frame, CharacterVector ease) {
  std::deque<NumericVector> tweendata;
  std::deque<std::string> tweengroup;
  std::deque<int> tweenframe;
  int i, j, nframes;
  std::string groupString;
  std::string currentGroup = as<std::string>(group[0]);

  for (i = 1; i < data.size(); ++i) {
    groupString = as<std::string>(group[i]);
    if (currentGroup.compare(groupString) == 0) {
      nframes = frame[i] - frame[i-1];
      std::vector<double> ease_points = easeSeq(as<std::string>(ease[i-1]), nframes);
      NumericVector state_from_vec = data[i - 1];
      NumericVector state_to_vec = data[i];
      state_from_vec = align_num_elem(state_from_vec, state_to_vec);
      state_to_vec = align_num_elem(state_to_vec, state_from_vec);
      for (j = 0; j < ease_points.size(); ++j) {
        NumericVector state_vec = state_from_vec + ease_points[j] * (state_to_vec - state_from_vec);
        tweendata.push_back(state_vec);
        tweengroup.push_back(groupString);
        tweenframe.push_back(j + frame[i-1]);
      }
    } else {
      tweendata.push_back(data[i - 1]);
      tweengroup.push_back(currentGroup);
      tweenframe.push_back(frame[i-1]);
      currentGroup = groupString;
    }

  }
  tweendata.push_back(data[i - 1]);
  tweengroup.push_back(currentGroup);
  tweenframe.push_back(frame[i-1]);
  List tweendata_list = wrap(tweendata);
  IntegerVector frame_vec = wrap(tweenframe);
  CharacterVector group_vec = wrap(tweengroup);
  List res = List::create(
    Named("data") = tweendata_list,
    Named("group") = group_vec,
    Named("frame") = frame_vec
  );
  res.attr("class") = "data.frame";
  res.attr("row.names") = seq_along(frame_vec);
  return res;
}

//[[Rcpp::export]]
DataFrame phase_element_interpolator(CharacterVector data, CharacterVector group, IntegerVector frame, CharacterVector ease) {
  std::deque<std::string> tweendata;
  std::deque<std::string> tweengroup;
  std::deque<int> tweenframe;
  int i, j, nframes;
  std::string groupString;
  std::string currentGroup = as<std::string>(group[0]);

  for (i = 1; i < data.size(); ++i) {
    groupString = as<std::string>(group[i]);
    if (currentGroup.compare(groupString) == 0) {
      nframes = frame[i] - frame[i-1];
      std::string type = data[i - 1] == "enter" ? "enter" : data[i] == "exit" ? "exit" : data[i - 1] == "static" ? "static" : "transition";
      for (j = 0; j < nframes; ++j) {
        if (j == 0 && (type == "transition" || type == "exit")) {
          tweendata.push_back("raw");
        } else {
          tweendata.push_back(type);
        }
        tweengroup.push_back(groupString);
        tweenframe.push_back(j + frame[i-1]);
      }
    } else {
      tweendata.push_back(as<std::string>(data[i - 1]));
      tweengroup.push_back(currentGroup);
      tweenframe.push_back(frame[i-1]);
      currentGroup = groupString;
    }

  }
  tweendata.push_back(as<std::string>(data[i - 1]));
  tweengroup.push_back(currentGroup);
  tweenframe.push_back(frame[i-1]);

  return DataFrame::create(
    Named("data") = wrap(tweendata),
    Named("group") = wrap(tweengroup),
    Named("frame") = wrap(tweenframe),
    Named("stringsAsFactors") = false
  );
}

//[[Rcpp::export]]
DataFrame numeric_along_interpolator(NumericVector data, CharacterVector group, NumericVector time, bool history, bool keep_last, int nframes, CharacterVector ease) {
  std::deque<double> tweendata;
  std::deque<std::string> tweengroup;
  std::deque<int> tweenframe;
  std::string easer = as<std::string>(ease);

  int i,j,jj;
  bool before,after,same,last;
  double pos,interp;

  for (i = 1; i <= nframes; ++i) {
    for (j = 0; j < data.size(); ++j) {
      last = j == data.size() - 1;
      jj = last ? j : j + 1;
      before = time[j] <= i;
      after = time[jj] > i;
      same = group[j] == group[jj];
      if ((history && same && before) || ((!same || last) && keep_last && before)) {
        tweendata.push_back(data[j]);
        tweengroup.push_back(as<std::string>(group[j]));
        tweenframe.push_back(i);
      }
      if (same && before == after) {
        pos = (i - time[j]) / (time[jj] - time[j]);
        pos = easePos(pos, easer);
        interp = data[j] + (data[jj] - data[j]) * pos;
        tweendata.push_back(interp);
        tweengroup.push_back(as<std::string>(group[j]));
        tweenframe.push_back(i);
      }
    }
  }

  return DataFrame::create(
    Named("data") = wrap(tweendata),
    Named("group") = wrap(tweengroup),
    Named("frame") = wrap(tweenframe),
    Named("stringsAsFactors") = false
  );
}
//[[Rcpp::export]]
DataFrame colour_along_interpolator(NumericMatrix data, CharacterVector group, NumericVector time, bool history, bool keep_last, int nframes, CharacterVector ease) {
  std::deque<double> tweendata1;
  std::deque<double> tweendata2;
  std::deque<double> tweendata3;
  std::deque<double> tweendata4;

  std::deque<std::string> tweengroup;
  std::deque<int> tweenframe;
  std::string easer = as<std::string>(ease);

  int i,j,jj;
  bool before,after,same,last;
  double pos;

  for (i = 1; i <= nframes; ++i) {
    for (j = 0; j < data.nrow(); ++j) {
      last = j == data.nrow() - 1;
      jj = last ? j : j + 1;
      before = time[j] <= i;
      after = time[jj] > i;
      same = group[j] == group[jj];
      if ((history && same && before) || ((!same || last) && keep_last && before)) {
        tweendata1.push_back(data(j, 0));
        tweendata2.push_back(data(j, 1));
        tweendata3.push_back(data(j, 2));
        tweendata4.push_back(data(j, 3));
        tweengroup.push_back(as<std::string>(group[j]));
        tweenframe.push_back(i);
      }
      if (same && before == after) {
        pos = (i - time[j]) / (time[j + 1] - time[j]);
        pos = easePos(pos, easer);
        tweendata1.push_back(data(j, 0) + (data(j + 1, 0) - data(j, 0)) * pos);
        tweendata2.push_back(data(j, 1) + (data(j + 1, 1) - data(j, 1)) * pos);
        tweendata3.push_back(data(j, 2) + (data(j + 1, 2) - data(j, 2)) * pos);
        tweendata4.push_back(data(j, 3) + (data(j + 1, 3) - data(j, 3)) * pos);
        tweengroup.push_back(as<std::string>(group[j]));
        tweenframe.push_back(i);
      }
    }
  }

  return DataFrame::create(
    Named("data1") = wrap(tweendata1),
    Named("data2") = wrap(tweendata2),
    Named("data3") = wrap(tweendata3),
    Named("data4") = wrap(tweendata4),
    Named("group") = wrap(tweengroup),
    Named("frame") = wrap(tweenframe),
    Named("stringsAsFactors") = false
  );
}

//[[Rcpp::export]]
DataFrame constant_along_interpolator(CharacterVector data, CharacterVector group, NumericVector time, bool history, bool keep_last, int nframes, CharacterVector ease) {
  std::deque<std::string> tweendata;
  std::deque<std::string> tweengroup;
  std::deque<int> tweenframe;
  std::string easer = as<std::string>(ease);

  int i,j,jj;
  bool before,after,same,last;
  double pos;

  for (i = 1; i <= nframes; ++i) {
    for (j = 0; j < data.size(); ++j) {
      last = j == data.size() - 1;
      jj = last ? j : j + 1;
      before = time[j] <= i;
      after = time[jj] > i;
      same = group[j] == group[jj];
      if ((history && same && before) || ((!same || last) && keep_last && before)) {
        tweendata.push_back(as<std::string>(data[j]));
        tweengroup.push_back(as<std::string>(group[j]));
        tweenframe.push_back(i);
      }
      if (same && before == after) {
        pos = (i - time[j]) / (time[j + 1] - time[j]);
        pos = easePos(pos, easer);
        if (pos < 0.5) {
          tweendata.push_back(as<std::string>(data[j]));
        } else {
          tweendata.push_back(as<std::string>(data[j + 1]));
        }
        tweengroup.push_back(as<std::string>(group[j]));
        tweenframe.push_back(i);
      }
    }
  }

  return DataFrame::create(
    Named("data") = wrap(tweendata),
    Named("group") = wrap(tweengroup),
    Named("frame") = wrap(tweenframe),
    Named("stringsAsFactors") = false
  );
}

//[[Rcpp::export]]
List list_along_interpolator(List data, CharacterVector group, NumericVector time, bool history, bool keep_last, int nframes, CharacterVector ease) {
  std::deque<SEXP> tweendata;
  std::deque<std::string> tweengroup;
  std::deque<int> tweenframe;
  std::string easer = as<std::string>(ease);

  int i,j,jj;
  bool before,after,same,last;
  double pos;

  for (i = 1; i <= nframes; ++i) {
    for (j = 0; j < data.size(); ++j) {
      last = j == data.size() - 1;
      jj = last ? j : j + 1;
      before = time[j] <= i;
      after = time[jj] > i;
      same = group[j] == group[jj];
      if ((history && same && before) || ((!same || last) && keep_last && before)) {
        tweendata.push_back(data[j]);
        tweengroup.push_back(as<std::string>(group[j]));
        tweenframe.push_back(i);
      }
      if (same && before == after) {
        pos = (i - time[j]) / (time[j + 1] - time[j]);
        pos = easePos(pos, easer);
        if (pos < 0.5) {
          tweendata.push_back(data[j]);
        } else {
          tweendata.push_back(data[j + 1]);
        }
        tweengroup.push_back(as<std::string>(group[j]));
        tweenframe.push_back(i);
      }
    }
  }

  List tweendata_list = wrap(tweendata);
  IntegerVector frame_vec = wrap(tweenframe);
  CharacterVector group_vec = wrap(tweengroup);
  List res = List::create(
    Named("data") = tweendata_list,
    Named("group") = group_vec,
    Named("frame") = frame_vec
  );
  res.attr("class") = "data.frame";
  res.attr("row.names") = seq_along(frame_vec);
  return res;
}
//[[Rcpp::export]]
List numlist_along_interpolator(List data, CharacterVector group, NumericVector time, bool history, bool keep_last, int nframes, CharacterVector ease) {
  std::deque<NumericVector> tweendata;
  std::deque<std::string> tweengroup;
  std::deque<int> tweenframe;
  std::string easer = as<std::string>(ease);

  int i,j,jj;
  bool before,after,same,last;
  double pos;

  for (i = 1; i <= nframes; ++i) {
    for (j = 0; j < data.size(); ++j) {
      last = j == data.size() - 1;
      jj = last ? j : j + 1;
      before = time[j] <= i;
      after = time[jj] > i;
      same = group[j] == group[jj];
      if ((history && same && before) || ((!same || last) && keep_last && before)) {
        tweendata.push_back(data[j]);
        tweengroup.push_back(as<std::string>(group[j]));
        tweenframe.push_back(i);
      }
      if (same && before == after) {
        NumericVector state_from_vec = data[j];
        NumericVector state_to_vec = data[j + 1];
        state_from_vec = align_num_elem(state_from_vec, state_to_vec);
        state_to_vec = align_num_elem(state_to_vec, state_from_vec);
        pos = (i - time[j]) / (time[j + 1] - time[j]);
        pos = easePos(pos, easer);
        NumericVector state_vec = state_from_vec + pos * (state_to_vec - state_from_vec);
        tweendata.push_back(state_vec);
        tweengroup.push_back(as<std::string>(group[j]));
        tweenframe.push_back(i);
      }
    }
  }

  List tweendata_list = wrap(tweendata);
  IntegerVector frame_vec = wrap(tweenframe);
  CharacterVector group_vec = wrap(tweengroup);
  List res = List::create(
    Named("data") = tweendata_list,
    Named("group") = group_vec,
    Named("frame") = frame_vec
  );
  res.attr("class") = "data.frame";
  res.attr("row.names") = seq_along(frame_vec);
  return res;
}
//[[Rcpp::export]]
DataFrame phase_along_interpolator(CharacterVector group, NumericVector time, bool history, bool keep_last, int nframes) {
  std::deque<std::string> tweendata;
  std::deque<std::string> tweengroup;
  std::deque<int> tweenframe;

  int i,j,jj;
  bool before,after,same,last;

  for (i = 1; i <= nframes; ++i) {
    for (j = 0; j < group.size(); ++j) {
      last = j == group.size() - 1;
      jj = last ? j : j + 1;
      before = time[j] <= i;
      after = time[jj] > i;
      same = group[j] == group[jj];
      if ((history && same && before) || ((!same || last) && keep_last && before)) {
        tweendata.push_back("raw");
        tweengroup.push_back(as<std::string>(group[j]));
        tweenframe.push_back(i);
      }
      if (same && before == after) {
        tweendata.push_back("transition");
        tweengroup.push_back(as<std::string>(group[j]));
        tweenframe.push_back(i);
      }
    }
  }

  return DataFrame::create(
    Named("data") = wrap(tweendata),
    Named("group") = wrap(tweengroup),
    Named("frame") = wrap(tweenframe),
    Named("stringsAsFactors") = false
  );
}

//[[Rcpp::export]]
NumericVector numeric_at_interpolator(NumericVector from, NumericVector to, NumericVector at, CharacterVector ease) {
  int n = from.size(), i;
  double pos;
  std::string easer = as<std::string>(ease);
  NumericVector res(n);

  for (i = 0; i < n; ++i) {
    pos = easePos(at[i], easer);
    res[i] = from[i] + (to[i] - from[i]) * pos;
  }

  return res;
}
//[[Rcpp::export]]
NumericMatrix colour_at_interpolator(NumericMatrix from, NumericMatrix to, NumericVector at, CharacterVector ease) {
  int n = from.nrow(), i;
  double pos;
  std::string easer = as<std::string>(ease);
  NumericMatrix res(n, from.ncol());

  for (i = 0; i < n; ++i) {
    pos = easePos(at[i], easer);
    res(i, _) = from(i, _) + (to(i, _) - from(i, _)) * pos;
  }

  return res;
}
//[[Rcpp::export]]
CharacterVector constant_at_interpolator(CharacterVector from, CharacterVector to, NumericVector at, CharacterVector ease) {
  int n = from.size(), i;
  double pos;
  std::string easer = as<std::string>(ease);
  CharacterVector res(n);

  for (i = 0; i < n; ++i) {
    pos = easePos(at[i], easer);
    res[i] = pos < 0.5 ? from[i] : to[i];
  }

  return res;
}
//[[Rcpp::export]]
List list_at_interpolator(List from, List to, NumericVector at, CharacterVector ease) {
  int n = from.size(), i;
  double pos;
  std::string easer = as<std::string>(ease);
  List res(n);

  for (i = 0; i < n; ++i) {
    pos = easePos(at[i], easer);
    res[i] = pos < 0.5 ? from[i] : to[i];
  }

  return res;
}
//[[Rcpp::export]]
List numlist_at_interpolator(List from, List to, NumericVector at, CharacterVector ease) {
  int n = from.size(), i;
  double pos;
  std::string easer = as<std::string>(ease);
  List res(n);

  for (i = 0; i < n; ++i) {
    NumericVector state_from_vec = from[i];
    NumericVector state_to_vec = to[i];
    state_from_vec = align_num_elem(state_from_vec, state_to_vec);
    state_to_vec = align_num_elem(state_to_vec, state_from_vec);
    pos = easePos(at[i], easer);
    NumericVector state_vec = state_from_vec + pos * (state_to_vec - state_from_vec);
    res[i] = state_vec;
  }

  return res;
}
//[[Rcpp::export]]
NumericVector numeric_fill_interpolator(NumericVector data, CharacterVector ease) {
  NumericVector res(data.size(), NA_REAL);
  int i,j,last = -1;
  std::string easer = as<std::string>(ease);
  std::vector<double> easepos;

  for (i = 0; i < data.size(); ++i) {
    if (NumericVector::is_na(data[i])) continue;
    if (last != -1) {
      easepos = easeSeq(easer, i - last);
      for (j = 1; j < easepos.size(); ++j) {
        res[last + j] = data[last] + easepos[j] * (data[i] - data[last]);
      }
    }
    res[i] = data[i];
    last = i;
  }

  return res;
}
//[[Rcpp::export]]
NumericMatrix colour_fill_interpolator(NumericMatrix data, CharacterVector ease) {
  NumericMatrix res(data.nrow(), data.ncol());
  std::fill(res.begin(), res.end(), NA_REAL);
  int i,j,last = -1;
  std::string easer = as<std::string>(ease);
  std::vector<double> easepos;

  for (i = 0; i < data.nrow(); ++i) {
    if (NumericVector::is_na(data(i, 0))) continue;
    if (last != -1) {
      easepos = easeSeq(easer, i - last);
      for (j = 1; j < easepos.size(); ++j) {
        res(last + j, _) = data(last, _) + easepos[j] * (data(i, _) - data(last, _));
      }
    }
    res(i, _) = data(i, _);
    last = i;
  }

  return res;
}
//[[Rcpp::export]]
CharacterVector constant_fill_interpolator(CharacterVector data, CharacterVector ease) {
  CharacterVector res(data.size(), NA_STRING);
  int i,j,last = -1;
  std::string easer = as<std::string>(ease);
  std::vector<double> easepos;

  for (i = 0; i < data.size(); ++i) {
    if (CharacterVector::is_na(data[i])) continue;
    if (last != -1) {
      easepos = easeSeq(easer, i - last);
      for (j = 1; j < easepos.size(); ++j) {
        res[last + j] = easepos[j] < 0.5 ? data[last] : data[i];
      }
    }
    res[i] = data[i];
    last = i;
  }

  return res;
}
//[[Rcpp::export]]
List list_fill_interpolator(List data, CharacterVector ease) {
  List res(data.size());
  int i,j,last = -1;
  std::string easer = as<std::string>(ease);
  std::vector<double> easepos;

  for (i = 0; i < data.size(); ++i) {
    if (data[i]==R_NilValue) continue;
    if (last != -1) {
      easepos = easeSeq(easer, i - last);
      for (j = 1; j < easepos.size(); ++j) {
        res[last + j] = easepos[j] < 0.5 ? data[last] : data[i];
      }
    }
    res[i] = data[i];
    last = i;
  }
  return res;
}
//[[Rcpp::export]]
List numlist_fill_interpolator(List data, CharacterVector ease) {
  List res(data.size());
  int i,j,last = -1;
  std::string easer = as<std::string>(ease);
  std::vector<double> easepos;

  for (i = 0; i < data.size(); ++i) {
    if (data[i]==R_NilValue) continue;
    if (last != -1) {
      easepos = easeSeq(easer, i - last);
      NumericVector state_from_vec = data[last];
      NumericVector state_to_vec = data[i];
      state_from_vec = align_num_elem(state_from_vec, state_to_vec);
      state_to_vec = align_num_elem(state_to_vec, state_from_vec);
      res[last] = data[last];
      for (j = 1; j < easepos.size(); ++j) {
        NumericVector state_vec = state_from_vec + easepos[j] * (state_to_vec - state_from_vec);
        res[last + j] = state_vec;
      }
    }
    res[i] = data[i];
    last = i;
  }
  return res;
}
