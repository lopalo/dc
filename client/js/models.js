
define(["underscore", "backbone"], function (_, Backbone) {
    var UI;
    var User;
    var Area;
    var Message;
    var Messages;
    var WorldmapItem;
    var Worldmap;

    function ReadOnlyProxy(model) {
        this._model = model;
        this.listenTo(model, "all", this._proxyEvent);
        if (model.proxyAttributes) {
            _.each(model.proxyAttributes, function (method) {
                var attr = model[method];
                if (_.isFunction(attr)) attr = attr.bind(model);
                this[method] = attr;
            }, this);
        }
    }
    _.extend(ReadOnlyProxy.prototype, Backbone.Events, {
        isInstanceOf: function (constructor) {
            return this._model instanceof constructor;
        },
        keys: function () {
            return this._model.keys();
        },
        values: function () {
            return this._model.values();
        },
        get: function (attr) {
            return this._model.get(attr);
        },
        previous: function (attr) {
            return this._model.previous(attr);
        },
        set: function () {
            throw "The model is read-only";
        },
        _proxyEvent: function () {
            var args = arguments;
            if (args[0] === "cleanup") this.stopListening();
            args[1] = this;
            this.trigger.apply(this, args);
        }
    });

    function ModelStore() {
        this.models = {};
        this.roModels = {};
    }
    ModelStore.prototype = {
        constructor: ModelStore,
        set: function (ident, model) {
            this.models[ident] = model;
            this.roModels[ident] = new ReadOnlyProxy(model);
        },
        del: function (ident) {
            delete this.models[ident];
            delete this.roModels[ident];
        },
        cleanup: function (ident) {
            this.models[ident].stopListening();
            this.models[ident].trigger("cleanup");
            this.del(ident);
        },
        cleanupAll: function () {
            _.each(_.keys(this.models), function (ident) {
                this.cleanup(ident);
            }, this);
        }
    };


    UI = Backbone.Model.extend({
        controlModes: ["view", "move", "shot"],
        windows: [null, "messages", "worldmap"],
        defaults: function () {
            return {
                controlModes: this.controlModes,
                controlMode: this.controlModes[1],
                windows: this.windows,
                activeWindow: this.windows[0],
                selectedObjectId: null,
                selectedObjectType: "nothing",
                selectedObjectInfo: {},
                selfObjectInfo: {}
            };
        }
    });


    User = Backbone.Model.extend({
        defaults: {
            userId: "",
            name: "",
        }
    });


    Area = Backbone.Model.extend({
        defaults: {
            areaId: null,
        }
    });


    Message = Backbone.Model.extend({
        idAttribute: "_id",
        defaults: function () {
            return {
                id: "",
                name: "",
                body: ""
            };
        }
    });


    Messages = Backbone.Collection.extend({
        model: Message,
        proxyAttributes: [
            "toArray",
        ],
    });


    WorldmapItem = Backbone.Model.extend({
        defaults: function () {
            return {
                owner: undefined,
                pos: [0, 0]
            };
        }
    });


    Worldmap = Backbone.Collection.extend({
        model: WorldmapItem,
        comparator: "id",
        proxyAttributes: [
            "getActive",
        ],
        getActive: function () {
            return _.chain(this.filter(function (i) {
                return i.get("owner") !== undefined;
            }));
        }
    });

    return {
        ReadOnlyProxy: ReadOnlyProxy,
        ModelStore: ModelStore,
        UI: UI,
        User: User,
        Area: Area,
        Messages: Messages,
        Worldmap: Worldmap
    };
});

