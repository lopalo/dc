define(["mithril", "utils"], function (m, utils) {

    var ClusterMesh = {
        controller: function () {
            return new utils.PollerController("/cluster/mesh");
        },
        view: function (ctrl) {
            var data = ctrl.response();
            var nodes = data.nodes;
            var mesh = data.mesh;
            var headers = nodes.map(function (node) {
                return m("th", node);
            });
            headers.unshift(m("th", ""));
            var thead = m("thead", m("tr", headers));

            var rows = nodes.map(function (node) {
                var row = nodes.map(function (targetNode) {
                    var connected;
                    if (node === targetNode) {
                        connected = "";
                    } else {
                        if (mesh[node] === undefined) {
                            connected = utils.boolIcon(false);
                        } else {
                            connected = utils.boolIcon(mesh[node][targetNode]);
                        }
                    }
                    return m("td", connected);
                });

                row.unshift(m("td", node));
                return m("tr", row);
            });
            var tbody = m("tbody", rows);
            var table = m("table.table stripped", [thead, tbody]);
            return table;
        },
    };

    return ClusterMesh;
});

