
define(["mithril", "utils"], function (m, utils) {

    var NodeStatistic = {
        controller: function () {
            return new utils.PollerController("/cluster/node-stats");
        },
        view: function (ctrl) {
            var data = ctrl.data();
            var fields = [
                m("dt", "Node Id"),
                m("dd", data["node-id"]),

                m("dt", "Registered Names"),
                m("dd", data["registered-names"]),

                m("dt", "Monitors"),
                m("dd", data.monitors),

                m("dt", "Links"),
                m("dd", data.links),

                m("dt", "Processes"),
                m("dd", data.processes),
            ];
            return m("dl.dl-horizontal col-md-4 col-md-offset-4", fields);
        }
    };

    return NodeStatistic;
});

