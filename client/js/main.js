
var connect;
var onOpen;
var onTimeout;
var initGame;
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
    connection.send("login", userName);
    connection.once("init", function (userData) {
        $("#connect-form").remove();
        initGame(connection, userData);
    });
    //TODO: show error if a user is already connected
    //TODO: process disconnection
}


function onTimeout(connection) {
    var error = $("#connect-error");
    error.html("Connection timeout");
    error.show();
    $("#connect").one("click", _.partial(connect, connection));
}

function initGame(connection, userData) {
    var gameEl = $("#game");
    var user = new User(userData);
    var area = new Area();
    var ui = new UI();
    gameEl.show();
    new World(gameEl.find("#viewport"), connection, user, area, ui);
    setupUI(gameEl.find("#ui"), ui, user, area);
}


