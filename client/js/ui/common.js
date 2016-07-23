
define(function (require) {
    var Backbone = require("backbone");
    require("bootstrap");
    require("bootstrap-select");


    var UIView;
    var Button;


    UIView = Backbone.View.extend({
        initialize: function () {
            this.listenTo(this.model, "cleanup", this.destroy);
        },
        destroy: function () {
            this.stopListening();
            this.undelegateEvents();
        }
    });


    Button = UIView.extend({
        events: {
            click: "click",
        },
        click: function () {
            this.trigger("click");
        },
    });


    return {
        UIView: UIView,
        Button: Button,
    };
});

