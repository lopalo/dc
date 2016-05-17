define(function (require) {
    var _ = require("underscore");
    var Backbone = require("backbone");

    function ObjectContextController(options) {
        this._models = options.models;
        this._connection = options.connection;
        this._stageController = options.stageController;
    }
    ObjectContextController.prototype = Object.create(Backbone.Events);
    _.extend(ObjectContextController.prototype, {
        listenToUI: function (views) {
            this.listenTo(views.areaSelector, "select", this._enterArea);
            this.listenTo(views.captureButton, "click", this._capture);
            this.listenTo(views.pullButton, "click", this._pullAsteroid);
        },
        destroy: function () {
            this.stopListening();
        },
        deleteStageObject: function (ident) {
        },
        getSelectedObjectId: function () {
            return this._models.models.ui.get("selectedObjectId");
        },
        _capture: function () {
            this._connection.send("area.capture", this.getSelectedObjectId());
        },
        _pullAsteroid: function () {
            this._connection.send("area.pull-asteroid",
                                  this.getSelectedObjectId());
        },
        _enterArea: function (areaId) {
            this._stageController.exitArea();
            this._connection.send("area.enter-area", areaId);
        },
    });

    return ObjectContextController;
});

