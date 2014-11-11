
var connect;
var onOpen;
var onTimeout;
var _connection; //use only for debugging

//TODO: use require.js

$(function () {
    var connection = new Connection();
    _connection = connection;
    $("#connect").one("click", _.partial(connect, connection));
    $("#connect").trigger("click"); //cleanup
});


function connect(connection) {
    $("#connect-error").hide();
    var address = "ws://" + $("#connect-address").val();
    connection.connect(address, 2000, onOpen, onTimeout);
}

function onOpen(connection) {
    var userName = $("#connect-name").val();
    var ui;
    $("#connect-form").remove();
    $("#game").show();
    ui = _.extend({}, Backbone.Events);
    new World($("#viewport"), connection, ui, userName);
    connection.send("login", userName);
    //TODO: connection.once("area.init", "remove the connect form")
    //TODO: show error if a user is already connected
    //TODO: process disconnection
}


function onTimeout(connection) {
    var error = $("#connect-error");
    error.html("Connection timeout");
    error.show();
    $("#connect").one("click", _.partial(connect, connection));
}



