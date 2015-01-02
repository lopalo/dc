
define(["backbone"], function (Backbone) {
    var User;
    var Area;

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
        User: User,
        Area: Area
    };
});

