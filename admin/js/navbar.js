

define(["bootstrap", "mithril", "pages"], function (_, m, pages) {

    var Navbar = {
        view: function () {
            var header = m(
                "li.sidebar-brand",
                m("a", {href: "/"}, "DC Admin")
            );
            var sectionItems = pages.map(function (section) {
                var active = m.route.param("section") === section.section;
                var buttonAttrs = {
                    href: "#" + section.section,
                    class: "nav-section" + (active ? " active" : ""),
                    "data-toggle": "collapse",
                    "aria-controls": section.section,
                    "aria-expanded": active
                };
                var button = m("a", buttonAttrs, section.title);

                var pageItems = section.pages.map(function (page) {
                    var route = "/" + section.section + "/" + page.id;
                    var active = m.route.param("page") === page.id;
                    var liClass = active ? ".active" : "";
                    var attrs = {config: m.route, href: route};
                    return m(
                        "li" + liClass,
                        m("a.nav-page", attrs, page.title)
                    );
                });

                var sectionAttrs = {
                    id: section.section,
                    class: "list-unstyled collapse" + (active ? " in" : "")
                };
                return m("li", [button, m("ul", sectionAttrs, pageItems)]);
            });
            sectionItems.unshift(header);
            return m("ul.sidebar-nav", sectionItems);
        }
    };

    return Navbar;
});

