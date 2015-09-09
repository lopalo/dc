
define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    var Connection = require("connection");
    var models = require("models");
    var UI = require("ui");
    var World = require("world/world");
    require("query-parser");


    function runApp() {
        var connection = new Connection();
        window._connection = connection; //use only for debugging
        $.getJSON("/ws-addresses").done(fillAddressField);
        $("#connect").one("click", _.partial(connect, connection));
    }

    function fillAddressField(addresses) {
        var el = $("#connect-address");
        var tmpl = _.template("<option value='<%= v %>'><%= v %></option>");
        var query = $.getQuery();
        _.each(addresses, function (pair) {
            el.append(tmpl({v: pair[0] + ":" + pair[1]}));
        });
        el.selectpicker();
        if (query.name) {
            $("#connect-name").val(query.name);
            $("#connect").trigger("click");
        }
    }

    function connect(connection) {
        $("#connect-error").hide();
        var address = "ws://" + $("#connect-address").val();
        connection.connect(address, 2000, onOpen, onTimeout);
    }

    function onOpen(connection) {
        var userName = $("#connect-name").val();
        connection.send("login", userName);
        window.history.pushState({}, '', "?name=" + userName);
        connection.once("init", function () {
            $("#connect-form").hide();
            initGame(connection);
        });
    }

    function onTimeout(connection) {
        $("#connect-error").html("Connection timeout").show();
        $("#connect").one("click", _.partial(connect, connection));
    }

    function initGame(connection) {
        var gameEl = $("#game");
        var user = new models.User();
        var area = new models.Area();
        var ui = new UI.UI();
        var world;
        connection.once("user.init", user.set, user);
        gameEl.show();
        world = new World(gameEl.find("#viewport"), connection, user, area, ui);
        UI.setupUI(gameEl.find("#ui"), ui, user, area);
        connection.once("disconnection", _.partial(onClose, connection, world));
    }

    function onClose(connection, world) {
        world.destroy();
        $("#game").hide();
        $("#connect-form").show();
        $("#connect-error").html("Disconnection").show();
        $("#connect").one("click", _.partial(connect, connection));
    }

    return runApp;
});


