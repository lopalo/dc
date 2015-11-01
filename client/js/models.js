
define(["underscore", "backbone"], function (_, Backbone) {
    var User;
    var Area;

    function set() {
        throw "The model is read-only";
    }

    function getReadOnlyProxy(model) {

        roModel = model; //TODO: use backbone-proxy

        //var roModel = Object.create(model);
        //roModel.set = set;
        //TODO: override destroy and other methods
        return roModel;
    }


    function ModelStore() {
        this.models = {};
        this.roModels = {};
    }
    ModelStore.prototype = {
        constructor: ModelStore,
        set: function (ident, model) {
            this.models[ident] = model;
            this.roModels[ident] = getReadOnlyProxy(model);
        },
        del: function (ident) {
            delete this.models[ident];
            delete this.roModels[ident];
        },
        destroy: function (ident) {
            this.models[ident].destroy();
            this.del(ident);
        },
        destroyAll: function () {
            _.each(this.models.keys(), function (ident) {
                this.destroy(ident);
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
        getReadOnlyProxy: getReadOnlyProxy,
        User: User,
        Area: Area
    };
});

