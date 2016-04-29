define(["mithril", "utils"], function (m, utils) {

    var AreaStatus = {
        controller: function () {
            return new utils.PollerController("/area/status");
        },
        view: function (ctrl) {
            var thead = m("thead", m("tr", [
                m("th", "Area"),
                m("th", "Users"),
                m("th", "Objects"),
                m("th", "Tick Duration (ms)"),
            ]));
            var summary = {
                users: 0,
                objects: 0,
                "tick-duration": 0
            };
            var rows = ctrl.data().map(function (record) {
                var name = m("td", record.id);
                var users = m("td", record.users);
                var objects = m("td", record.objects);
                var duration = m("td", record["tick-duration-ms"]);

                summary.users += record.users;
                summary.objects += record.objects;
                summary['tick-duration'] = Math.max(
                    summary['tick-duration'],
                    record["tick-duration-ms"]
                );

                return m("tr", [name, users, objects, duration]);
            });
            var summaryRow = [
                m("td", "Summary"),
                m("td", summary.users),
                m("td", summary.objects),
                m("td", summary["tick-duration"])
            ];
            rows.push(m("tr.active", summaryRow));
            var tbody = m("tbody", rows);
            var table = m("table.table stripped", [thead, tbody]);
            return table;
        },
    };

    return AreaStatus;
});

