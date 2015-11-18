
define(["mithril"], function (m) {

    var POLLING_INTERVAL = 1000;

    function extend (target, source) {
        Object.keys(source).map(function (k) {
            target[k] = source[k];
        });
    }

    function urlEncoded (xhr) {
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
                data: this.queryData
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
        updateQuery: function (url, queryData) {
            this._poller.url = url;
            this._poller.queryData = queryData;
        },
        onunload: function () {
            this._poller.stopPolling();
        },
    };

    return {
        extend: extend,
        urlEncoded: urlEncoded,
        Poller: Poller,
        PollerController: PollerController
    };
});

