
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

