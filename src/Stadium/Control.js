exports.mkControlImpl = (skip) => function (spec) {
  return function (setState) {
    return function (st) {
      return function (ac) {
        var setState_ = function (f) {
          var f_ = function (x) {
            if (x.type === st.type) {
              return f(x.value);
            } else {
              console.log(
                `WARNING: Dropping state update: Action ${ac.value.type} triggered from state ${st.type} but received in state ${x.type} with the following data: `,
                ac.value.value
              );
              return x;
            }
          };

          return setState(f_);
        };

        var setStateNoOp = function (f) {
          return setState((x) => x);
        };

        if (ac.type !== st.type) {
          console.log(
            `WARNING: Action ${ac.type}/${ac.value.type} was triggered from state ${st.type}`
          );
          return skip(st);
        }

        return spec[ac.type][ac.value.type](setState_)(st.value)(
          ac.value.value
        );
      };
    };
  };
};
