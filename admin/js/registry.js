
define(["mithril", "utils"], function (m, utils) {
    var PollerController = utils.PollerController;


    function RegistryController(url) {
        utils.bindAll(this, ["setPrefix", "setNode"]);
        PollerController.call(this, url, this._getQueryData());
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
        },
        getPrefix: function () {
            return localStorage.getItem("registry:prefix") || "";
        },
        setPrefix: function (prefix) {
            localStorage.setItem("registry:prefix", prefix);
            this._updateQuery();
        },
        getNode: function () {
            return localStorage.getItem("registry:node") || "";
        },
        setNode: function (node) {
            localStorage.setItem("registry:node", node);
            this._updateQuery();
        },
        _getQueryData: function () {
            return {prefix: this.getPrefix(), node: this.getNode()};
        },
        _updateQuery: function () {
            this._poller.queryData = this._getQueryData();
        },
    });


    var Registry = {
        controller: function () {
            return new RegistryController("/cluster/registry");
        },
        view: function (ctrl) {
            var prefixFilter = m(".form-group", m(".input-group", [
                m("span.input-group-addon", "Name Prefix"),
                m("input.form-control", {
                    oninput: m.withAttr("value", ctrl.setPrefix),
                    value: ctrl.getPrefix()
                })
            ]));
            var nodeFilter = m(".form-group", m(".input-group", [
                m("span.input-group-addon", "Node"),
                m("input.form-control", {
                    oninput: m.withAttr("value", ctrl.setNode),
                    value: ctrl.getNode()
                })

            ]));
            var filters = m("form.form-inline", [prefixFilter, nodeFilter]);


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
            var table = m("table.table stripped", [thead, tbody]);
            var page = m("div", [filters, table]);
            return page;
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

