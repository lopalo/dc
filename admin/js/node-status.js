
define(["mithril", "utils"], function (m, utils) {

    var NodeStatus = {
        controller: function () {
            return new utils.PollerController("/cluster/node-status");
        },
        view: function (ctrl) {
            function nodeView (data) {
                var stats = data.stats;
                var statViewBody = [
                    m("dt.key", "Node Id"),
                    m("dd", stats["node-id"]),

                    m("dt.key", "Registered Names"),
                    m("dd", stats["registered-names"]),

                    m("dt.key", "Monitors"),
                    m("dd", stats.monitors),

                    m("dt.key", "Links"),
                    m("dd", stats.links),

                    m("dt.key", "Processes"),
                    m("dd", stats.processes),
                ];
                var statsView = m(
                    "dl.dl-horizontal panel panel-info",
                    m(".panel-heading", "Statistic"),
                    m(".panel-body", statViewBody)
                );
                var baseViewBody = [
                    m("dt.key", "Global Registry"),
                    m("dd", utils.boolIcon(data["global-registry"])),

                    m("dt.key", "Logger"),
                    m("dd", utils.boolIcon(data.logger)),
                ];
                var baseView = m(
                    "dl.dl-horizontal panel panel-info",
                    m(".panel-heading", "Base Services"),
                    m(".panel-body", baseViewBody)
                );
                return m("div", [statsView, baseView]);
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
                return m(".panel panel-success node-status", [head, body]);
            });
            return m("div", viewList);
        },
    };

    return NodeStatus;
});

