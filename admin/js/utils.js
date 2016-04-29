
define(["mithril"], function (m) {

    var POLLING_INTERVAL = 1000;


    function extend(target, source) {
        //Partially emulates Object.assign from ES6
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


    function storageItem(key, defaultValue) {
        function setter(value) {
            if (value === undefined) {
                value = localStorage.getItem(key);
                if (value === null) {
                    return defaultValue;
                }
                return value;
            }
            localStorage.setItem(key, value);
        }
        return setter;
    }


    function Poller(url, queryData, pollingEnabledKey) {
        this.url = m.prop(url);
        this.queryData = m.prop(queryData);
        this.data = m.prop(null);
        this.pollingEnabled = storageItem(
            pollingEnabledKey || "polling-enabled",
            JSON.stringify(true)
        );
        this._intervalId = null;
    }
    Poller.prototype = {
        doRequest: function (force) {
            var enabled = JSON.parse(this.pollingEnabled());
            if (!enabled && this.data() !== null && !force) return;
            m.request({
                method: "GET",
                url: this.url(),
                data: this.queryData(),
                serialize: function (data) {
                    return m.route.buildQueryString(data);
                },
                config: urlEncoded
            }).then(this.data);
        },
        startPolling: function () {
            var func = this.doRequest.bind(this, false);
            this._intervalId = window.setInterval(func, POLLING_INTERVAL);
        },
        stopPolling: function () {
            window.clearInterval(this._intervalId);
        }
    };


    function PollerController(url, queryData, pollingEnabledKey) {
        var poller = new Poller(url, queryData, pollingEnabledKey);
        this._poller = poller;
        this.data = poller.data;
        this.pollingEnabled = poller.pollingEnabled;
        poller.startPolling();
        poller.doRequest();
        bindAll(this, ["togglePolling"]);
    }
    PollerController.prototype = {
        onunload: function () {
            this._poller.stopPolling();
        },
        togglePolling: function () {
            var newVal = !JSON.parse(this.pollingEnabled());
            this.pollingEnabled(JSON.stringify(newVal));
        },
        doRequest: function () {
            this._poller.doRequest(true);
        }
    };


    function boolIcon(value) {
        if (value) {
            return m("span.glyphicon glyphicon-ok text-success");
        }
        return m("span.glyphicon glyphicon-remove text-danger");
    }

    return {
        extend: extend,
        storageItem: storageItem,
        bindAll: bindAll,
        urlEncoded: urlEncoded,
        boolIcon: boolIcon,
        Poller: Poller,
        PollerController: PollerController
    };
});

