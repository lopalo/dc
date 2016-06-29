
define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    var Connection = require("connection");
    var Controller = require("controller");
    require("query-parser");


    function runApp() {
        $.getJSON("/ws-addresses").done(fillAddressField);
        $("#connect").one("click", connect);
    }

    function fillAddressField(addresses) {
        var el = $("#connect-address");
        var tmpl = _.template("<option value='<%= v %>'><%= n %></option>");
        var query = $.getQuery();
        _.each(addresses, function (item) {
            el.append(tmpl({n: item[0], v: item[1] + ":" + item[2]}));
        });
        el.selectpicker();
        if (query.name) {
            $("#connect-name").val(query.name);
            if (query.address) {
                el.selectpicker("val", query.address);
            }
            $("#connect").trigger("click");
        }
    }

    function connect() {
        var connection = new Connection();
        window._connection = connection; //use only for debugging
        $("#connect-error").hide();
        var address = "ws://" + $("#connect-address").val();
        connection.connect(address, 2000, onOpen, onTimeout);
    }

    function onOpen(connection) {
        var userName = $("#connect-name").val();
        var address = $("#connect-address").val();
        var queryString = "?name=" + userName + "&address=" + address;
        connection.send("login", userName);
        window.history.pushState({}, '', queryString);
        connection.once("init", function () {
            $("#connect-form").hide();
            initGame(connection);
        });
        connection.once("disconnection", _.partial(onClose, connection));
        connection.once("error", connection.close);
    }

    function onTimeout() {
        $("#connect-error").html("Connection timeout").show();
        $("#connect").one("click", connect);
    }

    function initGame(connection) {
        var controller = new Controller($("#game"), connection);
        controller.init();
        controller.start();
        connection.once("disconnection", controller.destroy);
    }

    function onClose(connection) {
        var error = connection.lastError || "Disconnection";
        $("#connect-form").show();
        $("#connect-error").html(error).show();
        $("#connect").one("click", connect);
    }

    return runApp;
});


