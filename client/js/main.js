
var _connection; //use only for debugging

$(function () {
    var connection = new Connection();
    _connection = connection;
    $("#connect").one("click", _.partial(connect, connection));
});


function connect(connection) {
    $("#connect-error").hide();
    var address = "ws://" + $("#connect-address").val();
    connection.connect(address, 2000, onOpen,
                       _.partial(onTimeout, connection));
}

function onOpen() {
    var name = $("#connect-name").val();
    $("#connect-form").remove();
    $("#viewport").show();
    //TODO: create world
}


function onTimeout(connection) {
    var error = $("#connect-error");
    error.html("Connection timeout");
    error.show();
    $("#connect").one("click", _.partial(connect, connection));
}



