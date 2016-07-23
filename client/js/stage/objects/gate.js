
define(function (require) {
    var models = require("stage/models");
    var views = require("stage/views");

    var Gate;
    var GateView;

    Gate = models.StageObject.extend({
        objectType: "gate",
        getInfoForUI: function () {
            return {
                name: this.get("name"),
            };
        },
    });


    GateView = views.StageObject.extend({
        texturePath: "/client/img/gate.png",
    });

    return {
        Gate: Gate,
        GateView: GateView
    };

});

