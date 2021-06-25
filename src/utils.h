#pragma once

#include <vector>
#include <numeric>
#include "cpp11/doubles.hpp"
#include "easing.h"

enum Easer {
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
static inline Easer get_easer(std::string ease) {
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

static inline std::vector<double> ease_seq(std::string easer, int length) {
  std::vector<double> res(length);
  double p;
  // Just linear for now
  for(int i = 0; i < length; ++i) {
    p = double(i) / length;
    switch (get_easer(easer)) {
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
      cpp11::stop("Unknown easing function");
    }
  }
  return res;
}
static inline double ease_pos(double p, std::string easer) {
  double p_new = 0;
  switch (get_easer(easer)) {
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
    cpp11::stop("Unknown easing function");
  }
  return p_new;
}

static inline cpp11::doubles align_num_elem(cpp11::doubles from, cpp11::doubles to) {
  if (from.size() < to.size()) {
    cpp11::writable::doubles res(to.size());
    if (from.size() == 0) {
      double mean = std::accumulate(to.begin(), to.end(), 0.0) / to.size();
      std::fill(res.begin(), res.end(), mean);
      return res;
    }
    for (int i = 0; i < res.size(); ++i) {
      res[i] = from[i % from.size()];
    }
    return res;
  }
  return from;
}
