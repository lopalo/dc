
define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");
    var TweenLite = require("tween-lite");

    var models = require("models");
    var UI = require("ui");
    var StageController = require("stage/controller");


    function Controller(gameEl, connection) {
        this._gameEl = gameEl;
        this._connection = connection;
        this._models = new models.ModelStore();
        this._models.set("user", new models.User());
        this._models.set("area", new models.Area());
        this._models.set("ui", new UI.UI());
        this._stageController = new StageController(gameEl.find("#viewport"),
                                                    this);
        _.bindAll(this, "destroy");

    }
    Controller.prototype = Object.create(Backbone.Events);
    _.extend(Controller.prototype, {
        constructor: Controller,
        init: function () {
            var uiViews;
            var stage = this._stageController;
            var conn = this._connection;
            var user = this._models.models.user;
            var roModels = this._models.roModels;
            conn.once("user.init", user.set, user);
            uiViews = UI.setupUI(this._gameEl.find("#ui"),
                                 roModels.ui,
                                 roModels.user,
                                 roModels.area);
            this.listenTo(conn, "area.init", this._initArea);
            this._listenToUI(uiViews);
            stage.init();
            stage.listenTo(conn, "area", stage.dispatch);
            this._gameEl.show();
        },
        start: function () {
            this._stageController.start();
        },
        destroy: function () {
            this._gameEl.hide();
            this.stopListening();
            this._models.destroyAll();
            this._stageController.destroy();
        },
        listenToStageBackground: function (bg) {
            //TODO: "mouseDown", "mouseUp", "mouseMove"
        },
        listenToStageObjectView: function (view) {
            this.listenTo(view, "click", this._clickStageView);
        },
        request: function () {
            this._connection.apply(this._connection, arguments);
        },
        getAreaModel: function () {
            return this._models.roModels.area;
        },
        getUserId: function () {
            return this._models.roModels.user.get("userId");
        },
        isSelf: function (id) {
            return this.getUserId() === id;
        },
        _listenToUI: function (uiViews) {
            this.listenTo(uiViews.focusButton, "click", this._showMyself);
            this.listenTo(uiViews.igniteButton, "click", this._ignite);
            this.listenTo(uiViews.areaSelector, "select", this._enterArea);
            this.listenTo(uiViews.controlModeSelector, "select",
                                        this._changeControlMode);
        },
        _showMyself: function () {
            this._stageController.showMyself();
        },
        _ignite: function () {
            this._connection.send("area.ignite", 10);
        },
        _enterArea: function (areaId) {
            this._stageController.enterArea();
            this._connection.send("area.enter-area", areaId);
        },
        _changeControlMode: function (mode) {
            this._ui.set("controlMode", mode);
        },
        _initArea: function (data) {
            this._models.models.area.set({
                areaId: data.areaId,
                background: data.areaId + ".jpg" //FIXME
            });
        },
        _clickStageView: function (ident) {
            var model = this._stageController.getObjectModel(ident);
            //TODO: make a shot when
            //      appropriate control mode is chosen
            this.connection.send("area.shoot", ident);
        },
        _moveAlongRoute: function () {
            var positions = _.map(route, function (pos) {
                return pos.toArray();
            });
            this.connection.send("area.move-along-route", positions);
        },
    });
    return Controller;
});

