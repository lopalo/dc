
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
                m("th", "Node"),
                m("th", "Uptime"),
                m("th", "Kill"),
            ]));
            var rows = ctrl.data().map(function (record) {
                var name = m("td", record[0]);
                var nodeId = m("td", record[1]);
                var uptime = m("td", formatTimeDelta(record[2]));

                var killBtnAttrs = {
                    class: "btn btn-default",
                    onclick: ctrl.killProcess.bind(ctrl, record[0])
                };
                var killSpan = m("span.glyphicon glyphicon-remove");
                var killBtn = m("button", killBtnAttrs, killSpan);
                var kill = m("td", killBtn);

                return m("tr", [name, nodeId, uptime, kill]);
            });
            var tbody = m("tbody", rows);
            return m("table.table stripped", [thead, tbody]);
        }
    };

    function formatTimeDelta(ms) {
        var dayMs = 8.64 * Math.pow(10, 7);
        var hourMs = 3.6 * Math.pow(10, 6);
        var minuteMs = 60000;
        var secondMs = 1000;
        var str = [];
        ms = Math.floor(ms);
        var days = Math.floor(ms / dayMs);
        ms %= dayMs;
        if (days) {
            str += days + "-";
        }
        var hours = Math.floor(ms / hourMs).toString();
        ms %= hourMs;
        str += (hours.length === 1 ? "0" + hours : hours) + ":";
        var minutes = Math.floor(ms / minuteMs).toString();
        ms %= minuteMs;
        str += (minutes.length === 1 ? "0" + minutes : minutes) + ":";
        var seconds = Math.floor(ms / secondMs).toString();
        ms %= secondMs;
        str += seconds.length === 1 ? "0" + seconds : seconds;
        return str;
    }

    return Registry;
});

