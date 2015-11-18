
define(["mithril", "utils"], function (m, utils) {
    var PollerController = utils.PollerController;

    function RegistryController() {
        PollerController.apply(this, arguments);
    }
    RegistryController.prototype = Object.create(PollerController.prototype);
    utils.extend(RegistryController.prototype, {
        killProcess: function (name) {
            m.request({
                method: "POST",
                url: "/cluster/kill-process-by-name",
                data: {name: name},
                serialize: function (data) {
                    return m.route.buildQueryString(data);
                },
                config: utils.urlEncoded
            });
        }
    });

    var Registry = {
        controller: function () {
            return new RegistryController("/cluster/registry");
        },
        view: function (ctrl) {
            var thead = m("thead", m("tr", [
                m("th", "Name"),
                m("th", "Node ID"),
                m("th", "Kill"),
            ]));
            var rows = ctrl.data().map(function (record) {
                var name = m("td", record[0]);
                var nodeId = m("td", record[1]);

                var killBtnAttrs = {
                    class: "btn btn-default",
                    onclick: ctrl.killProcess.bind(ctrl, record[0])
                };
                var killSpan = m("span.glyphicon glyphicon-remove");
                var killBtn = m("button", killBtnAttrs, killSpan);
                var kill = m("td", killBtn);

                return m("tr", [name, nodeId, kill]);
            });
            var tbody = m("tbody", rows);
            return m("table.table stripped", [thead, tbody]);
        }
    };

    return Registry;
});

