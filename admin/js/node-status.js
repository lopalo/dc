
define(["mithril", "utils"], function (m, utils) {

    var NodeStatus = {
        controller: function () {
            return new utils.PollerController("/cluster/node-status");
        },
        view: function (ctrl) {
            function nodeView (data) {
                var stats = data.stats;
                var fields = [
                    m("dt", "Node Id"),
                    m("dd", stats["node-id"]),

                    m("dt", "Registered Names"),
                    m("dd", stats["registered-names"]),

                    m("dt", "Monitors"),
                    m("dd", stats.monitors),

                    m("dt", "Links"),
                    m("dd", stats.links),

                    m("dt", "Processes"),
                    m("dd", stats.processes),

                    m("dt", "Global Registry"),
                    m("dd", data["global-registry"]),

                    m("dt", "Global Cache"),
                    m("dd", data["global-cache"]),

                    m("dt", "Logger"),
                    m("dd", data.logger),
                ];
                return m("dl.dl-horizontal", fields);
            }
            var nodeStatusMap = ctrl.data();
            var viewList = Object.keys(nodeStatusMap).map(function (name) {
                var nodeStatus = nodeStatusMap[name];
                var head = m(".panel-heading", name);
                var body;
                if (nodeStatus === null) {
                    return m(".panel panel-danger node-status", [head]);
                }
                body = m(".panel-body", nodeView(nodeStatus));
                return m(".panel panel-info node-status", [head, body]);
            });
            return m("div", viewList);
        },
    };

    return NodeStatus;
});

