
define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    var Connection = require("connection");
    var Controller = require("controller");
    require("query-parser");


    function runApp() {
        $("#auth .selectpicker").selectpicker();
        $.getJSON("/ws-addresses").done(fillAddressField);
        listenToSubmit();
    }

    function listenToSubmit() {
        $("#submit-login").one("click", _.partial(connect, false));
        $("#submit-signup").one("click", _.partial(connect, true));
    }

    function fillAddressField(addresses) {
        var el = $(".server-address");
        var tmpl = _.template("<option value='<%= v %>'><%= n %></option>");
        var query = $.getQuery();
        _.each(addresses, function (item) {
            el.append(tmpl({n: item[0], v: item[1] + ":" + item[2]}));
        });
        el.selectpicker();
        if (query.login) {
            $("#login-form #login").val(query.login);
            if (query.address) {
                el.selectpicker("val", query.address);
            }
            $("#submit-login").trigger("click");
        }
    }

    function connect(signUp) {
        var connection = new Connection();
        window._connection = connection; //use only for debugging
        $("#error").hide();
        var selector = signUp ? "#signup-form" : "#login-form";
        var address = "ws://" + $(selector + " #server-address").val();
        connection.connect(address, 2000, _.partial(onOpen, signUp), onTimeout);
    }

    function onOpen(signUp, connection) {
        var selector, address, login, password;
        connection.once("disconnection", _.partial(onClose, connection));
        connection.once("error", connection.close);
        if (signUp) {
            selector = "#signup-form ";
            address = $(selector + "#server-address").val();
            login = $(selector + "#login").val().trim();
            var name = $(selector + "#name").val().trim();
            var asset = $(selector + "#asset").val();
            password = $(selector + "#password").val().trim();
            var confirmPassword = $(selector + "#confirm-password").val();
            if (!login || !name) {
                return connection.error("Login and name are required");
            }
            if (login.length < 6 || login.length > 12) {
                return connection.error("Login length must be from 6 to 12");
            }
            if (!/^[a-z0-9]+$/i.test(login)) {
                return connection.error("Login must be alphanumeric");
            }
            if (password !== confirmPassword) {
                return connection.error(
                    "Password doesn't match the confirm password"
                );
            }
            connection.send("sign-up", {
                login: login,
                name: name,
                asset: asset,
                password
            });
        } else {
            selector = "#login-form ";
            address = $(selector + "#server-address").val();
            login = $(selector + "#login").val();
            password = $(selector + "#password").val();
            connection.send("log-in", [login, password]);
        }

        var queryString = "?login=" + login + "&address=" + address;
        window.history.pushState({}, "", queryString);
        connection.once("init", function () {
            $("#auth").hide();
            initGame(connection);
        });
    }

    function onTimeout() {
        $("#error").html("Connection timeout").show();
        listenToSubmit();
    }

    function initGame(connection) {
        var controller = new Controller($("#game"), connection);
        controller.init();
        controller.start();
        connection.once("disconnection", controller.destroy);
    }

    function onClose(connection) {
        var error = connection.lastError || "Disconnection";
        $("#auth").show();
        $("#error").html(error).show();
        listenToSubmit();
    }

    return runApp;
});


