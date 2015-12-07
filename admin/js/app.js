
define(function (require) {
    var m = require("mithril");
    var Navbar = require("navbar");
    var pages = require("pages");

    function init() {
        var routes = {};

        pages.forEach(function (page) {
            routes[page.route] = page.component;
        });
        m.mount(document.getElementById("navbar"), Navbar);
        m.route(document.getElementById("page"), "registry", routes);
        m.route.mode = "hash";
    }

    return init;
});

