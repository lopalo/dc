define(function (require) {
    var _ = require("underscore");
    var Backbone = require("backbone");

    function WindowsController(options) {
        this._models = options.models;
        this._connection = options.connection;
        this._stageController = options.stageController;
    }
    WindowsController.prototype = Object.create(Backbone.Events);
    _.extend(WindowsController.prototype, {
        listenToUI: function (views) {
            this.listenTo(views.closeButton, "activateWindow",
                                            this._activateWindow);
            this.listenTo(views.messagesButton, "activateWindow",
                                            this._activateWindow);
            this.listenTo(views.messagesWindow, "send", this._sendMessage);
            this.listenTo(views.worldmapButton, "activateWindow",
                                            this._activateWindow);
        },
        destroy: function () {
            this.stopListening();
        },
        deleteStageObject: function () {
        },
        _activateWindow: function (windowName) {
            this._models.models.ui.set("activeWindow", windowName);
        },
        _sendMessage: function (message) {
            var userIds = this._stageController.getVisibleUserIds();
            this._connection.send("user.send-message", [userIds, message]);
        },
    });

    return WindowsController;
});


