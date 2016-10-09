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
    NumericMatrix res(nelements * nframes, 3);

    for (state = 0; state < nstates; ++state) {
        if (easer[state] == "constant") {
            NumericMatrix state_from = data(state_index(state));
            for (currentframe = 0; currentframe < nframes_per_state(state); ++currentframe) {
                res_index = (frame + currentframe) * nelements;
                for (element = 0; element < nelements; ++element) {
                    res(res_index, 0) = state_from(element, 0);
                    res(res_index, 1) = state_from(element, 1);
                    res(res_index, 2) = state_from(element, 2);
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
        Named("frame") = wrap(tweenframe)
    );
}

//[[Rcpp::export]]
DataFrame colour_element_interpolator(NumericMatrix data, CharacterVector group, IntegerVector frame, CharacterVector ease) {
    std::deque<double> tweendata1;
    std::deque<double> tweendata2;
    std::deque<double> tweendata3;
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
                tweengroup.push_back(groupString);
                tweenframe.push_back(j + frame[i-1]);
            }
        } else {
            tweendata1.push_back(data(i - 1, 0));
            tweendata2.push_back(data(i - 1, 1));
            tweendata3.push_back(data(i - 1, 2));
            tweengroup.push_back(currentGroup);
            tweenframe.push_back(frame[i-1]);
            currentGroup = groupString;
        }
    }

    return DataFrame::create(
        Named("data1") = wrap(tweendata1),
        Named("data2") = wrap(tweendata1),
        Named("data3") = wrap(tweendata1),
        Named("group") = wrap(tweengroup),
        Named("frame") = wrap(tweenframe)
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
        Named("frame") = wrap(tweenframe)
    );
}
