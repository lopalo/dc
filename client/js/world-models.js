
var Area;
var Camera;
var WorldObject;
var Unit;
var CAMERA_SPEED = 10; //px per second

Area = Backbone.Model.extend({
    defaults: {
        id: null,
        background: null
    }
});

Camera = Backbone.Model.extend({
    defaults: {
        x: 0,
        y: 0,
        height: 0,
        width: 0
    },
    moveTo: function (direction) {
        this.set({
            x: this.get("x") + direction[0] * CAMERA_SPEED,
            y: this.get("y") + direction[1] * CAMERA_SPEED
        });
    }
});


WorldObject = Backbone.Model.extend({
    defaults: {
        pos: [0, 0],
        height: 80,
        width: 40,
        angle: 0
    },
});


Unit = WorldObject.extend({
    defaults: _.extend({
        name: "unit"
    }, WorldObject.prototype.defaults)
});

