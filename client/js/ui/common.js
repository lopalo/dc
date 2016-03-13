
define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    var Backbone = require("backbone");
    require("bootstrap");
    require("bootstrap-select");


    var UIView;
    var Button;
    var WindowButton;
    var CloseButton;


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


    WindowButton = UIView.extend({
        events: {
            click: "click",
        },
        initialize: function (options) {
            WindowButton.__super__.initialize.call(this);
            this.windowName = options.windowName;
            this.listenTo(this.model, "change:activeWindow", this.render);
            this.render();
        },
        render: function () {
            this.$el.toggle(this.model.get("activeWindow") === null);
        },
        click: function () {
            this.trigger("activateWindow", this.windowName);
        },
    });


    CloseButton = WindowButton.extend({
        initialize: function () {
            CloseButton.__super__.initialize.call(this, {windowName: null});
        },
        render: function () {
            this.$el.toggle(this.model.get("activeWindow") !== null);
        },
    });

    return {
        UIView: UIView,
        Button: Button,
        WindowButton: WindowButton,
        CloseButton: CloseButton
    };
});

