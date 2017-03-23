
define(["mithril", "utils", "validation"], function (m, utils, v) {

    function ServiceSettingsModel() {
        utils.bindAll(this, ["setType"]);
        this.nodes = m.prop([]);
        this.reset();
    }
    ServiceSettingsModel.prototype = {
        serviceTypes: [
            "area-db",
            "db",
            "ws",
            "http",
            "admin",
            "area",
            "log-aggregator"
        ],
        reset: function () {
            this.id = m.prop("");
            this.node = m.prop("");
            this.type = m.prop("");
            this.options = m.prop({});
        },
        setType: function (type) {
            var options;
            this.type(type);
            var hostPort = {host: m.prop(""), port: m.prop("")};
            switch (type) {
                case "http":
                    options = hostPort;
                    break;
                case "admin":
                    options = hostPort;
                    break;
                case "ws":
                    options = hostPort;
                    break;
                case "area":
                    options = {"min-db-replicas": m.prop(0)};
                    break;
                default:
                    options = {};
            }
            this.options(options);
        },
        validate: function () {
            var errors = {};
            var optErrors = {};
            var opts = this.options();

            var notEmpty;
            var number;
            var positive;

            notEmpty = v.notEmpty(this, errors);

            notEmpty("id");
            notEmpty("node");
            notEmpty("type");

            notEmpty = v.notEmpty(opts,  optErrors);
            number = v.number(opts, optErrors);
            positive = v.positive(opts, optErrors);
            if (opts.host !== undefined)
                notEmpty("host");
            if (opts.port !== undefined) {
                notEmpty("port");
                number("port");
                positive("port");
            }
            if (opts["min-db-replicas"] !== undefined) {
                notEmpty("min-db-replicas");
                number("min-db-replicas");
                positive("min-db-replicas");
            }

            if (!utils.isEmpty(optErrors)) {
                errors.options = optErrors;
            }
            return errors;
        },

        toJSON: function () {
            var settings =  {
                type: this.type,
                ident: this.id,
            };
            utils.extend(settings, this.options());
            return [this.node, settings];
        }
    };


    function FormController() {
        utils.bindAll(this, ["submit"]);
        this.serviceSettings = new ServiceSettingsModel();
        this.submitted = m.prop(false);
        this.showErrors = m.prop(false);
        m.request({
            method: "GET",
            url: "/cluster/node-status",
        }).then(function (data) {
            var res = [];
            data.forEach(function (pair) {
                if (pair[1] !== null) {
                    res.push(pair[0]);
                }
            });
            return res;
        }).then(this.serviceSettings.nodes);
    }
    FormController.prototype = {
        submit: function () {
            var submitted = this.submitted;
            var s = this.serviceSettings;
            if (!utils.isEmpty(s.validate())) {
                this.showErrors(true);
                return;
            }
            submitted(true);
            m.request({
                method: "POST",
                url: "/cluster/start-service",
                data: s,
            }).then(function () { submitted(false); });
            s.reset();
            this.showErrors(false);
            m.redraw();
        }
    };


    var StartServiceForm = {
        controller: function () {
            return new FormController();
        },
        view: function (ctrl) {
            var s = ctrl.serviceSettings;
            var errors = ctrl.showErrors() ? s.validate() : {};

            var msgClass = ctrl.submitted() ? "" : " disappearing";
            var message = m(".form-group", m(
                ".alert alert-info" + msgClass,
                "Service is starting ..."
            ));
            var id = v.validationFormGroup(errors.id, [
                m("label.control-label", "Service ID"),
                m("input.form-control", {
                    oninput: m.withAttr("value", s.id),
                    value: s.id()
                }),
            ]);
            var node = v.validationFormGroup(errors.node, [
                m("label.control-label", "Node"),
                m("select.form-control", {
                    onchange: m.withAttr("value", s.node),
                    value: s.node()
                }, s.nodes().map(option)),
            ]);
            var type = v.validationFormGroup(errors.type, [
                m("label.control-label", "Service Type"),
                m("select.form-control", {
                    onchange: m.withAttr("value", s.setType),
                    value: s.type()
                }, s.serviceTypes.map(option)),
            ]);
            var submit = m(".form-group", m("button.btn btn-default", {
                disabled: !utils.isEmpty(errors),
                type: "button",
                onclick: ctrl.submit,
            }, "Start Service"));
            var optionsView = this._optionsView(
                s.type(),
                s.options(),
                errors.options || {}
            );
            return m(".form-horizontal col-xs-4 col-xs-offset-4", [
                message,
                node,
                id,
                type,
                optionsView,
                submit
            ]);
        },
        _optionsView: function (type, opts, optErrors) {
            var fields;
            function createHostPortFields() {
                return [
                    v.validationFormGroup(optErrors.host, [
                        m("label.control-label", "Host"),
                        m("input.form-control", {
                            oninput: m.withAttr("value", opts.host),
                            value: opts.host()
                        }),
                    ]),
                    v.validationFormGroup(optErrors.port, [
                        m("label.control-label", "Port"),
                        m("input.form-control", {
                            type: "number",
                            oninput: m.withAttr("value", opts.port),
                            value: opts.port()
                        }),
                    ]),
                ];
            }
            switch (type) {
                case "http":
                    fields = createHostPortFields();
                    break;
                case "admin":
                    fields = createHostPortFields();
                    break;
                case "ws":
                    fields = createHostPortFields();
                    break;
                case "area":
                    fields = [
                        v.validationFormGroup(optErrors["min-db-replicas"], [
                            m("label.control-label", "Minimum DB Replicas"),
                            m("input.form-control", {
                                type: "number",
                                oninput: m.withAttr(
                                    "value",
                                    opts["min-db-replicas"]
                                ),
                                value: opts["min-db-replicas"]()
                            }),
                        ])
                    ];
                    break;
                default:
                    fields = null;
            }
            if (fields === null) return "";
            return m("fieldset", [m("legend", "Options"), m("div", fields)]);
        }
    };


    function option(value) {
        return m("option", {value: value}, value);
    }

    return StartServiceForm;
});

