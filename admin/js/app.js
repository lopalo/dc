
define(function (require) {
    var m = require("mithril");
    var Navbar = require("navbar");
    var pages = require("pages");


    var App = {
        view: function () {
            var sectionName = m.route.param("section");
            var pageName = m.route.param("page");
            var component;
            pages.forEach(function (section) {
                if (section.section === sectionName) {
                    section.pages.forEach(function (page) {
                        if (page.id === pageName) {
                            component = page.component;
                        }
                    });
                }
            });
            return component;
        }
    };


    function init() {
        m.route.mode = "hash";
        m.route(
            document.getElementById("page"),
            "/cluster/node-status",
            {"/:section/:page": App}
        );
        m.mount(document.getElementById("navbar"), Navbar);
    }

    return init;
});


