

define(["mithril", "pages"], function (m, pages) {

    var Navbar = {
        view: function () {
            var items = pages.map(function (page) {
                var liClass = m.route() === page.route ? "active" : "";
                var attrs = {config: m.route, href: page.route};
                return m("li", {class: liClass}, m("a", attrs, page.title));
            });
            return m("ul.nav navbar-nav", items);
        }
    };

    return Navbar;
});

