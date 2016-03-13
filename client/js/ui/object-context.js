define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    var Backbone = require("backbone");
    require("bootstrap");
    require("bootstrap-select");

    var common = require("ui/common");


    var SelectArea;
    var CaptureButton;
    var PullButton;
    var ObjectInfo;

    SelectArea = common.UIView.extend({
        events: {
            change: "change",
        },
        template: _.template("<option value='<%= v %>'><%= n %></option>"),
        initialize: function (options) {
            SelectArea.__super__.initialize.call(this);
            this.area = options.area;
            this.user = options.user;
            this.listenTo(this.area, "change:areaId", this.render);
            this.listenTo(this.user, "change:areas", this.render);
            this.listenTo(this.model, "change:selectedObjectType", this.render);
            this.render();
        },
        render: function () {
            var el = this.$el;
            var gateSelected = this.model.get("selectedObjectType") === "gate";
            this.$el.parent().toggle(gateSelected);
            if (!gateSelected) return;
            el.empty();
            _.each(this.user.get("areas"), function (aid) {
                el.append(this.template({
                    v: aid,
                    n: aid.replace("area:", "")
                }));
            }, this);
            this.$el.val(this.area.get("areaId"));
            this.$el.selectpicker("refresh");
        },
        change: function () {
            this.trigger("select", this.$el.val());
        },
    });


    CaptureButton = common.Button.extend({
        initialize: function () {
            CaptureButton.__super__.initialize.call(this);
            this.listenTo(this.model, "change:selectedObjectType", this.render);
            this.listenTo(this.model, "change:selectedObjectInfo", this.render);
            this.render();
        },
        render: function () {
            var type = this.model.get("selectedObjectType");
            var info = this.model.get("selectedObjectInfo");
            this.$el.toggle(type === "control-point" && info.owner === null);
        }
    });


    PullButton = common.Button.extend({
        initialize: function () {
            PullButton.__super__.initialize.call(this);
            this.listenTo(this.model, "change:selectedObjectType", this.render);
            this.listenTo(this.model, "change:selectedObjectInfo", this.render);
            this.render();
        },
        render: function () {
            var type = this.model.get("selectedObjectType");
            var info = this.model.get("selectedObjectInfo");
            this.$el.toggle(type === "asteroid" && info.pullAllowed);
        }
    });


    ObjectInfo = common.UIView.extend({
        initialize: function () {
            ObjectInfo.__super__.initialize.call(this);
            this.listenTo(this.model, "change:selectedObjectInfo", this.render);
        },
        render: function () {
            var el = this.$el;
            var type = this.model.get("selectedObjectType");
            var info = this.model.get("selectedObjectInfo");
            if (!_.isEmpty(info)) {
                var tmpl = $("#" + type + "-info-template");
                if (tmpl) {
                    el.html(_.template(tmpl.html())(info));
                    el.show();
                }
            } else {
                el.hide();
            }
        }
    });

    return {
        SelectArea: SelectArea,
        CaptureButton: CaptureButton,
        PullButton: PullButton,
        ObjectInfo: ObjectInfo,
    };
});

