define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    var Backbone = require("backbone");
    require("bootstrap");
    require("bootstrap-select");
    var settings = require("json!settings.json");

    var common = require("ui/common");

    var WorldmapWindow = common.UIView.extend({
        headerTemplate: _.template(
            '<div class="panel-heading">' +
                'World Owner: <%= owner === null ? "-" : owner %>' +
            '</div>'
        ),
        initialize: function (options) {
            WorldmapWindow.__super__.initialize.call(this);
            this.worldmap = options.worldmap;
            this.area = options.area;
            this.listenTo(this.model, "change:activeWindow", this.render);
            this.listenTo(this.area, "change:areaId", this.render);
            this.listenTo(this.worldmap, "update", this.render);
            this.render();
        },
        render: function () {
            var isActive = this.model.get("activeWindow") === "worldmap";
            var el = this.$el;
            var panel = $("<div>").addClass("pane panel-info");
            var items = this.worldmap.getActive();
            var worldOwner = null;
            var half = Math.floor(items.size().value() / 2);
            var mapEl;
            el.toggle(isActive);
            if (!isActive) return;
            el.empty();
            items.map(function (i) {
                return i.get("owner");
            })
            .filter()
            .countBy()
            .pairs()
            .each(function (ownerCount) {
                if (ownerCount[1] > half) {
                    worldOwner = ownerCount[0];
                }
            });
            panel.append(this.headerTemplate({owner: worldOwner}));
            mapEl = $("<div>").addClass("panel-body ui-worldmap");
            this._createMap(items).appendTo(mapEl);
            panel.append(mapEl);
            panel.appendTo(el);
        },
        _createMap: function (items) {
            var el = $("<div>").addClass("worldmap-field");
            var minX = 0;
            var minY = 0;
            var maxX = 0;
            var maxY = 0;
            var width = 0;
            var height = 0;
            items.each(function (i) {
                var pos = i.get("pos");
                minX = Math.min(pos[0], minX);
                maxX = Math.max(pos[0], maxX);
                minY = Math.min(pos[1], minY);
                maxY = Math.max(pos[1], maxY);
            });
            width = maxX - minX;
            height = maxY - minY;
            el.css({width: width, height: height});
            items.each(function (i) {
                var pos = i.get("pos");
                var x = pos[0];
                var y = pos[1];
                var title = settings.areas[i.id].title;
                var owner = i.get("owner");
                if (owner !== null) {
                    title += " (" + owner + ")";
                }
                x -= minX;
                y -= minY;
                var item = $("<div>")
                    .addClass("worldmap-item")
                    .css({left: x, bottom: y});
                if (i.id === this.area.get("areaId")) {
                    item.addClass("current");
                }
                $("<span>").text(title).appendTo(item);
                $("<div>").addClass("worldmap-circle").appendTo(item);
                el.append(item);
            }, this);
            return el;
        },
    });

    return WorldmapWindow;
});

