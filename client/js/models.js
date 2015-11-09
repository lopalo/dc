
define(["underscore", "backbone"], function (_, Backbone) {
    var User;
    var Area;

    function ReadOnlyProxy(model) {
        this._model = model;
        this.listenTo(model, "all", this._proxyEvent);
    }
    _.extend(ReadOnlyProxy.prototype, Backbone.Events, {
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


    User = Backbone.Model.extend({
        defaults: {
            userId: "",
            name: "",
            areas: []
        }
    });


    Area = Backbone.Model.extend({
        defaults: {
            areaId: null,
            background: null
        }
    });

    return {
        ReadOnlyProxy: ReadOnlyProxy,
        ModelStore: ModelStore,
        User: User,
        Area: Area
    };
});

