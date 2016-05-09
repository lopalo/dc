
define(["mithril"], function (m) {

    function validator(predicate, error, parser) {
        parser = parser || function (v) { return v; };
        return function (container, errors) {
            return function (fieldName) {
                var field = container[fieldName];
                if (!errors[fieldName] && predicate(field())) {
                    field(parser(field()));
                } else {
                    errors[fieldName] = errors[fieldName] || error;
                }
            };
        };
    }


    var notEmpty = validator(
        function (v) { return v !== ""; },
        "Empty field"
    );


    var number = validator(
        function (v) { return !isNaN(v); },
        "Is not a number",
        parseFloat
    );


    var positive = validator(
        function (v) { return v > 0; },
        "Must be positive"
    );


    function validationFormGroup (error, elements) {
        var help = [];
        if (error) {
            help = [m("span.help-block", error)];
        }
        return m(
            ".form-group" + (error ? " has-error" : ""),
            elements.concat(help)
        );
    }


    return {
        validator: validator,
        notEmpty: notEmpty,
        number: number,
        positive: positive,
        validationFormGroup: validationFormGroup
    };
});


