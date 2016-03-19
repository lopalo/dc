
define(["mithril"], function (m) {

    var POLLING_INTERVAL = 1000;

    function extend(target, source) {
        Object.keys(source).forEach(function (k) {
            target[k] = source[k];
        });
    }

    function bindAll(obj, keys) {
        keys.forEach(function (k) {
            obj[k] = obj[k].bind(obj);
        });
    }

    function urlEncoded(xhr) {
        xhr.setRequestHeader(
            "Content-type",
            "application/x-www-form-urlencoded"
        );
    }


    function Poller(url, queryData) {
        this.data = m.prop(null);
        this.url = url;
        this.queryData = queryData;
        this._intervalId = null;
    }
    Poller.prototype = {
        doRequest: function () {
            m.request({
                method: "GET",
                url: this.url,
                data: this.queryData,
                serialize: function (data) {
                    return m.route.buildQueryString(data);
                },
                config: urlEncoded
            }).then(this.data);
        },
        startPolling: function () {
            var func = this.doRequest.bind(this);
            this._intervalId = window.setInterval(func, POLLING_INTERVAL);
        },
        stopPolling: function () {
            window.clearInterval(this._intervalId);
        }
    };

    function PollerController(url, queryData) {
        var poller = this._poller = new Poller(url, queryData);
        this.data = poller.data;
        poller.startPolling();
        poller.doRequest();
    }
    PollerController.prototype = {
        onunload: function () {
            this._poller.stopPolling();
        },
    };

    function boolIcon(value) {
        if (value) {
            return m("span.glyphicon glyphicon-ok text-success");
        }
        return m("span.glyphicon glyphicon-remove text-danger");
    }

    return {
        extend: extend,
        bindAll: bindAll,
        urlEncoded: urlEncoded,
        boolIcon: boolIcon,
        Poller: Poller,
        PollerController: PollerController
    };
});

