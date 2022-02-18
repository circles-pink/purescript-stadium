exports.mkControlImpl = function (spec) {
    return function (setState) {
        return function (st) {
            return function (ac) {
                return spec[st.type][ac.value.type](setState)(st.value)(ac.value.value)
            }
        }
    }
}