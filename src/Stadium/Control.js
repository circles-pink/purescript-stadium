exports.mkControlImpl = function (spec) {
    return function (setState) {
        return function (st) {
            return function (ac) {
                var setState_ = function (f) {
                    var f_ = function (x) {
                        return f(x.value)
                    }
                    return setState(f_)
                };
                return spec[st.type][ac.value.type](setState_)(st.value)(ac.value.value)
            }
        }
    }
}